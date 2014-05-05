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

  def packageName = ssd.packageName

  override def src: String = {
    s"""package object $packageName {
  import scala.language.reflectiveCalls // avoid warnings from generated code

  object Client {
    private val apiToken = sys.props.getOrElse(
      "$packageName.api.token",
      sys.error("API token must be provided")
    )

    private val apiUrl = sys.props.getOrElse(
      "$packageName.api.url",
      sys.error("API URL must be provided")
    )

    private val logger = play.api.Logger("$packageName.Client")

    def requestHolder(resource: String) = {
      val url = apiUrl + resource
      play.api.libs.ws.WS.url(url).withAuth(apiToken, "", com.ning.http.client.Realm.AuthScheme.BASIC)
    }
  }

  trait Client {
    import Client._

    def resource: String

    protected def requestHolder(path: String) = Client.requestHolder(resource + path)

    private def logRequest(method: String, req: play.api.libs.ws.WS.WSRequestHolder)(implicit ec: scala.concurrent.ExecutionContext): play.api.libs.ws.WS.WSRequestHolder = {
      // auth should always be present, but just in case it isn't,
      // we'll supply a default
      val (apiToken, _, _) = req.auth.getOrElse(("", "", com.ning.http.client.Realm.AuthScheme.BASIC))
      logger.info(s"curl -X $$method -u '[REDACTED]:' $${req.url}")
      req
    }

    private def processResponse(f: scala.concurrent.Future[play.api.libs.ws.Response])(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[play.api.libs.ws.Response] = {
      f.map { response =>
        logger.debug(response.body)
        response
      }
    }

    protected def POST[T](path: String, data: play.api.libs.json.JsValue)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[play.api.libs.ws.Response] = {
      processResponse(logRequest("POST", requestHolder(path)).post(data))
    }

    protected def GET(path: String, q: Seq[(String, String)])(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[play.api.libs.ws.Response] = {
      processResponse(logRequest("GET", requestHolder(path).withQueryString(q:_*)).get())
    }

    protected def PATCH[T](path: String, data: play.api.libs.json.JsValue)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[play.api.libs.ws.Response] = {
      throw new UnsupportedOperationException // TODO support PATCH
    }

    protected def PUT[T](path: String, data: play.api.libs.json.JsValue)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[play.api.libs.ws.Response] = {
      processResponse(logRequest("PUT", requestHolder(path)).put(data))
    }

    protected def DELETE[T](path: String)(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[play.api.libs.ws.Response] = {
      processResponse(logRequest("DELETE", requestHolder(path)).delete())
    }

    case class HandlerNotSpecified(msg: String) extends RuntimeException(msg)
  }

${body.indent}
}"""
  }

  def body: String = {
    val resourceDefs = resources.map(_.src)
    (Seq(ssd.modelTypeDef) ++ resourceDefs).mkString("\n")
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
        val dataType = param.dataType match {
          case ref: ScalaDataType.Reference => ref.field.dataType
          case t => t
        }
        val writer = dataType.play2JsonWriter
        if (param.isOption) {
        s""""${param.originalName}" -> ({ value: ${param.typeName} =>
  value.map { value: ${dataType.name} =>
${writer.indent(4)}
  }.getOrElse(play.api.libs.json.JsNull)
})(${param.name})"""
        } else {
        s""""${param.originalName}" -> ({ value: ${param.typeName} =>
${writer.indent}
})(${param.name})"""
        }
      }.mkString(",\n").indent
s"""val payload = play.api.libs.json.Json.obj(
$objArgs
)"""
    }

    def buildGetArgs: String = {
      val builder = List.newBuilder[String]
      builder += "val qBuilder = List.newBuilder[(String, String)]"
      operation.parameters.foreach { param =>
        // TODO better logic for transforming the parameter value into a string
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
        s"""status${code}: ${typeName} => T = { value: ${typeName} =>
  val json = play.api.libs.json.Json.prettyPrint(${response.dataType.play2JsonWriter})
  throw new HandlerNotSpecified(s"unhandled response code $code with body: $${json}")
}"""
      }
      val defaults = Seq(
        """status204: Unit => T = { _: Unit => throw new HandlerNotSpecified("unhandled resposne code 204") }""",
        """status304: Unit => T = { _: Unit => throw new HandlerNotSpecified("unhandled resposne code 304") }""",
        """default: play.api.libs.ws.Response => T = { r: play.api.libs.ws.Response => throw new HandlerNotSpecified(s"unexpected response code ${r.status} with body ${r.body}") }"""
      )
      (generated ++ defaults).sorted.mkString("\n", ",\n", "\n").indent(4)
    }

    def applyBody = {
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
  status${status}(({ json: play.api.libs.json.JsValue =>
${response.dataType.play2JsonReader.indent(4)}
  })(r.json))"""
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

    def argList: String = {
      parameters.map {
        // for reference use the type of the field being pointed at
        case param if param.isReference =>
          val realType = param.dataType.asInstanceOf[ScalaDataType.Reference].field.dataType
          val typeName = if (param.isOption) s"Option[${realType.name}] = None" else realType.name
          s"${param.name}: $typeName"
        case param => param.src
      }.mkString(",\n")
    }

    def typeDefs: String = {
      // TODO generate types for parameters
      val paramTypeDefs = parameters.flatMap { param =>
        import ScalaDataType._
        param.dataType match {
          case r: Reference =>
            val underlyingType = r.field.dataType
            Some(s"""type ${param.name.capitalize} = ${underlyingType.name}""")
          case ut: UserType =>
            val resourceName = ut.resource.name
            Some(generateModelClass(param.name.capitalize, ut.fields))
            // TODO support nested lists
          case List(_, ut: UserType) =>
            val resourceName = ut.resource.name
            Some(generateModelClass(param.name.capitalize, ut.fields))
          case _ => None
        }
      }
      val responseTypeDefs = responses.flatMap { response =>
        import ScalaDataType._
        response.dataType match {
          case ut: UserType =>
            val resourceName = ut.resource.name
            // TODO better name for generated type (possibly include the resourceName)
            Some(generateModelClass("Status" + response.code, ut.fields))
            // TODO support nested lists
          case List(_, ut: UserType) =>
            val resourceName = ut.resource.name
            Some(generateModelClass("Status" + response.code, ut.fields))
          case _ => None
        }
      }
      (paramTypeDefs ++ responseTypeDefs).mkString("\n", "\n", "\n")
    }

    override def src: String = s"""
${description}object $name {${typeDefs.indent}
  def apply[T](
${argList.indent(4)}
  )($handlerList
  )(implicit ec: scala.concurrent.ExecutionContext): scala.concurrent.Future[T] = {
${applyBody.indent(4)}
  }
}"""
  }

  // TODO move type defs into ScalaServiceDescription
  def generateModelClass(name: String, fields: Seq[ScalaField]): String = {

    def fieldDefs = {
      val generated = fields.map { field =>
        val typeName = field match {
          case field if field.isUserType =>
            s"$name.${field.name.capitalize}"
          case field =>
            field.dataType.name
        }
        s"def ${field.name}: ${field.typeName}"
      }
      generated.mkString("\n")
    }

    def companionDef: String = {
      def typeRefDefs: Seq[String] = {
        fields.flatMap { field =>
          import ScalaDataType._
          def generateTypeForUserType(field: ScalaField, dataType: UserType): Option[String] = {
            val isPartial = dataType.fields.size != dataType.resource.fields.size
            if (isPartial) {
              Some {
                generateModelClass(
                  name = field.name.capitalize,
                  fields = dataType.fields)
              }
            } else {
              Some(s"type ${field.name.capitalize} = $packageName.${dataType.resource.name}")
            }
          }
          field.dataType match {
            case dataType: UserType => generateTypeForUserType(field, dataType)
            // TODO support nested lists
            case List(_, dataType: UserType) => generateTypeForUserType(field, dataType)
            case _ => None
          }
        }
      }
      typeRefDefs.map(_.indent) match {
        case Nil => ""
        case defs => defs.mkString(s"object ${name} {\n", "\n", "\n}")
      }
    }

s"""
type $name = {
${fieldDefs.indent}
}

${companionDef}"""
  }

  case class Resource(resource: ScalaResource) extends Source {
    import resource._

    def operations: Seq[Source] = resource.operations.map(Operation(_))

    override def src: String = s"""
${generateModelClass(resource.name, fields)}

object ${resource.name}Client extends Client {
  def resource = "$path"
$clientBody
}"""

    def clientBody: String = {
      val methods = operations.map(_.src.indent).mkString("\n")
      methods
    }
  }
}

