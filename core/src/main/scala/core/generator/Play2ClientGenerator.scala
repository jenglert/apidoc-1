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

  import play.api.libs.ws.{Response => PlayResponse, _}
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

    private def processResponse(f: Future[PlayResponse])(implicit ec: ExecutionContext): Future[PlayResponse] = {
      f.map { response =>
        logger.debug(response.body)
        response
      }
    }

    protected def POST[T](path: String, data: JsValue)(implicit ec: ExecutionContext): Future[PlayResponse] = {
      processResponse(logRequest("POST", requestHolder(path)).post(data))
    }

    protected def GET(path: String, q: Seq[(String, String)])(implicit ec: ExecutionContext): Future[PlayResponse] = {
      processResponse(logRequest("GET", requestHolder(path).withQueryString(q:_*)).get())
    }

    protected def PATCH[T](path: String, data: JsValue)(implicit ec: ExecutionContext): Future[PlayResponse] = {
      throw new UnsupportedOperationException // TODO support PATCH
    }

    protected def PUT[T](path: String, data: JsValue)(implicit ec: ExecutionContext): Future[PlayResponse] = {
      processResponse(logRequest("PUT", requestHolder(path)).put(data))
    }

    protected def DELETE[T](path: String)(implicit ec: ExecutionContext): Future[PlayResponse] = {
      processResponse(logRequest("DELETE", requestHolder(path)).delete())
    }
  }

  case class Response[T](status: Int, entity: T)
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

    def body = {
      def methodCall = method.toUpperCase match {
        case "POST" => s"""$buildPayload
POST(
  path = $pathArg,
  payload
)"""

        case "GET" => s"""$buildGetArgs
GET(
  path = $pathArg,
  qBuilder.result
)"""

        case "PATCH" => s"""$buildPayload
PATCH(
  path = $pathArg,
  payload
)"""

        case "PUT" => s"""$buildPayload
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
          s"case r if r.status == ${status} => Response(r.status, r.json.as[$typeName])"
        }.mkString("\n")
        s""".map {
${cases}
  case r if r.status == 204 => Response(204, ()) // 204 has no body
  case r if r.status == 304 => Response(304, ()) // 304 has no body
  case r => Response(r.status, r.body)
}"""
      }
      methodCall + responseProcessing
    }

    override def src: String = s"""
${description}def $name($argList)(implicit ec: ExecutionContext): Future[Response[_]] = {
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

case class ${name}(${argList})

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
  def reads(value: ${resource.name}) = {
    Json.obj(
${writeFields.indent(6)}
    )
  }
}
"""
    }
  }
}

