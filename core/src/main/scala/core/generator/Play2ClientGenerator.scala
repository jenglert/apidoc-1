package core.generator

import core._
import Text._
import ScalaUtil._

object Play2ClientGenerator {
  def apply(json: String) = {
    val sd = ServiceDescription(json)
    val ssd = new ScalaServiceDescription(sd)
    new Play2ClientGenerator(ssd).src
  }
}

class Play2ClientGenerator(ssd: ScalaServiceDescription)
extends Source
{
  def resources = ssd.resources.map(Resource(_))

  def packageName = ssd.name.toLowerCase

  override def src: String = {
    s"""package $packageName

object ${ssd.name} {

  import scala.concurrent.ExecutionContext
  import scala.concurrent.Future

  import com.ning.http.client.Realm.AuthScheme

  import play.api.libs.ws._
  import play.api.libs.ws.WS.WSRequestHolder
  import play.api.libs.json._
  import play.api.libs.functional.syntax._
  import play.api.Logger

  import java.util.UUID
  import org.joda.time.DateTime

  object Client {
    private val apiToken = sys.props.getOrElse(
      "$packageName.api.token",
      sys.error("API token must be provided")
    )

    private val apiUrl = sys.props.getOrElse(
      "$packageName.api.url",
      sys.error("API URL must be provided")
    )

    private val logger = Logger("$packageName.${ssd.name}.Client")

    def requestHolder(resource: String) = {
      val url = apiUrl + resource
      WS.url(url).withAuth(apiToken, "", AuthScheme.BASIC)
    }
  }

  trait Client {
    import Client._

    def resource: String

    protected def requestHolder(path: String) = Client.requestHolder(resource + path)

    private def logRequest(method: String, req: WSRequestHolder)(implicit ec: ExecutionContext): WSRequestHolder = {
      // auth should always be present, but just in case it isn't,
      // we'll supply a default
      val (apiToken, _, _) = req.auth.getOrElse(("", "", AuthScheme.BASIC))
      logger.info(s"curl -X $$method -u '[REDACTED]:' $${req.url}")
      req
    }

    private def processResponse(f: Future[Response])(implicit ec: ExecutionContext): Future[Response] = {
      f.map { response =>
        logger.debug(response.body)
        response
      }
    }

    protected def POST[T](path: String, data: JsValue)(implicit ec: ExecutionContext): Future[Response] = {
      processResponse(logRequest("POST", requestHolder(path)).post(data))
    }

    protected def GET(path: String, q: Seq[(String, String)])(implicit ec: ExecutionContext): Future[Response] = {
      processResponse(logRequest("GET", requestHolder(path).withQueryString(q:_*)).get())
    }

    protected def PATCH[T](path: String, data: JsValue)(implicit ec: ExecutionContext): Future[Response] = {
      throw new UnsupportedOperationException // TODO support PATCH
    }

    protected def PUT[T](path: String, data: JsValue)(implicit ec: ExecutionContext): Future[Response] = {
      processResponse(logRequest("PUT", requestHolder(path)).put(data))
    }

    protected def DELETE[T](path: String)(implicit ec: ExecutionContext): Future[Response] = {
      processResponse(logRequest("DELETE", requestHolder(path)).delete())
    }

    case class HandlerNotSpecified(msg: String) extends RuntimeException(msg)
  }
$body
}"""
  }

  def body: String = {
    val jsonFormatDefs = {
      s"""
  implicit val jsonReadsUUID: Reads[UUID] = __.read[String].map(UUID.fromString)

  implicit val jsonWritesUUID = new Writes[UUID] {
    override def writes(value: UUID) = {
      JsString(value.toString)
    }
  }

  import org.joda.time.format.ISODateTimeFormat

  private val iso8601 = ISODateTimeFormat.basicDateTime

  implicit val jsonDateTimeReads: Reads[DateTime] = {
    __.read[String].map(iso8601.parseDateTime)
  }

  implicit val jsonDateTimeWrites = new Writes[DateTime] {
    override def writes(value: DateTime) = {
      JsString(iso8601.print(value))
    }
  }
"""
    }
    val resourceDefs = resources.map(_.src.indent).mkString("\n")
    jsonFormatDefs ++ resourceDefs
  }

  case class Operation(operation: ScalaOperation) extends Source {
    import operation._

    def pathArg: String = {
      val tmp = path.map("^/:".r.replaceAllIn(_, ""))
        .getOrElse("")
      if (tmp.isEmpty) {
        "\"\""
      } else {
        if (tmp.startsWith("/")) "\"" + tmp + "\"" else "\"/\" + \"" + tmp + "\""
      }
    }

    def buildPayload: String = {
      def objArgs = operation.parameters.map { param =>
        s""""${param.originalName}" -> Json.toJson(${param.name})"""
      }.mkString(",\n").indent
s"""val payload = Json.obj(
$objArgs
)"""
    }

    def buildGetArgs: String = {
      val builder = List.newBuilder[String]
      builder += "val qBuilder = List.newBuilder[(String, String)]"
      operation.parameters.foreach { param =>
        if (param.isOption) {
          builder += s"""qBuilder ++= ${param.name}.map("${param.originalName}" -> _.toString)"""
        } else {
          builder += s"""qBuilder += "${param.originalName}" -> ${param.name}.toString"""
        }
      }
      builder.result.mkString("\n")
    }

    def handlerList: String = {
      val generated = responses.map { response =>
        val code = response.code
        val typeName = response.dataType.name
        s"""status${code}: ${typeName} => T = { result: ${typeName} =>
  val json = Json.prettyPrint(Json.toJson(result))
  throw new HandlerNotSpecified(s"unhandled response code $code with body: $${json}")
}"""
      }
      val defaults = Seq(
        """status204: Unit => T = { _: Unit => throw new HandlerNotSpecified("unhandled resposne code 204") }""",
        """status304: Unit => T = { _: Unit => throw new HandlerNotSpecified("unhandled resposne code 304") }""",
        """default: Response => T = { r: Response => throw new HandlerNotSpecified(s"unexpected response code ${r.status} with body ${r.body}") }"""
      )
      (generated ++ defaults).sorted.mkString("\n", ",\n", "\n").indent
    }

    def body = {
      def methodCall = method.toUpperCase match {
        case "POST" => s"""
$buildPayload
POST(
  path = $pathArg,
  payload
)"""

        case "GET" => s"""
$buildGetArgs
GET(
  path = $pathArg,
  qBuilder.result
)"""

        case "PATCH" => s"""
$buildPayload
PATCH(
  path = $pathArg,
  payload
)"""

        case "PUT" => s"""
$buildPayload
PUT(
  path = $pathArg,
  payload
)"""
        case "DELETE" => s"""DELETE($pathArg)"""
      }

      def responses = operation.responses.sortBy(_.code)

      def responseProcessing: String = {
        def cases: String = responses.map { response =>
          val status = response.code
          val typeName = response.dataType.name
          s"""case r if r.status == ${status} =>
  status${status}(r.json.as[$typeName])"""
        }.mkString("\n")
        s""".map {
${cases.indent}
  case r if r.status == 204 => status204(())
  case r if r.status == 304 => status304(())
  case r => default(r)
}"""
      }
      methodCall + responseProcessing
    }

    override def src: String = s"""
${description}def $name[T]($argList)($handlerList)(implicit ec: ExecutionContext): Future[T] = {
${body.indent}
}
"""
  }
  case class Resource(resource: ScalaResource) extends Source {
    import resource._

    def operations: Seq[Source] = resource.operations.map(Operation(_))

    override def src: String = s"""
object ${resource.name} {
  $companionBody
}

case class $name($argList)

object ${resource.name}Client extends Client {
  def resource = "$path"
$clientBody
}
"""

    def clientBody: String = {
      val methods = operations.map(_.src.indent).mkString("\n")
      methods
    }

    def companionBody: String = {
s"""
  implicit val reads: Reads[${resource.name}] = ${jsonReads.indent(4)}

  implicit val writes: Writes[${resource.name}] = ${jsonWrites.indent(4)}
"""
    }

    def jsonReads: String = {
      def readFields = {
        resource.fields.map { field =>
          val typeName = field.dataType.name
          if (field.isOption) {
            s"""${field.name} = (json \\ "${field.originalName}").asOpt[${typeName}]"""
          } else {
            s"""${field.name} = (json \\ "${field.originalName}").as[${typeName}]"""
          }
        }.mkString(",\n")
      }
s"""
new Reads[${resource.name}] {
  def reads(json: JsValue) = JsSuccess {
    new ${resource.name}(
${readFields.indent(6)}
    )
  }
}
"""
    }

    def jsonWrites: String = {
      def writeFields = {
        resource.fields.map { field =>
          s""""${field.originalName}" -> Json.toJson(value.${field.name})"""
        }.mkString(",\n")
      }
s"""
new Writes[${resource.name}] {
  def writes(value: ${resource.name}) = {
    Json.obj(
${writeFields.indent(6)}
    )
  }
}
"""
    }
  }
}

