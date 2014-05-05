package core

import Text._

object ScalaUtil {
  def textToComment(text: String) = {
    val lines = text.split("\n")
    lines.mkString("/**\n * ", "\n * ", "\n */\n")
  }
}

import ScalaUtil._

class ScalaServiceDescription(serviceDescription: ServiceDescription)
{
  def name = safeName(serviceDescription.name)

  def description = serviceDescription.description.map(textToComment).getOrElse("")

  def resources = serviceDescription.resources.map { resource =>
    new ScalaResource(resource, packageName)
  }

  def packageName = name.toLowerCase

  def modelTypeDef = {
s"""trait Model {
  /**
   * A link where the resource instance can be found.
   * This may be returned by a server, and should be
   * ignored if provided by a client.
   */
  def _self: Option[java.net.URL] = None
}

trait ModelView[T] extends Model""".indent
  }
}

class ScalaResource(resource: Resource, packageName: String)
{
  def name = singular(underscoreToInitCap(resource.name))

  def fullName = packageName + "." + name

  def path = resource.path

  def description = resource.description.map(textToComment).getOrElse("")

  def fields = resource.fields.map(new ScalaField(_, fullName))

  def operations = resource.operations.map { operation =>
    // we assume that all client stuff goes into an object suffixed by Client
    // This means we never have to worry about name collisions between what
    // we put in the companion and the operations of the client API
    new ScalaOperation(operation, fullName + "Client")
  }
}

class ScalaOperation(operation: Operation, packageName: String)
{
  def method = operation.method

  def path = operation.path

  def fullName = packageName + "." + name

  def description = operation.description.map(textToComment).getOrElse("")

  def parameters = operation.parameters.map { new ScalaField(_, fullName) }

  def name = method.toLowerCase + path.map(safeName).getOrElse("").capitalize

  def responseTypeName = name.capitalize

  def responses = operation.responses.map(new ScalaResponse(_, fullName))
}

class ScalaField(field: Field, packageName: String) extends Source {
  def name: String = snakeToCamelCase(field.name)

  def companionName = packageName + "." + name.capitalize

  def originalName: String = field.name

  def dataType: ScalaDataType = ScalaDataType(field.dataType, field.format, companionName)

  def isReference = classOf[ScalaDataType.Reference].isAssignableFrom(dataType.getClass)

  def isUserType = classOf[ScalaDataType.UserType].isAssignableFrom(dataType.getClass)

  def description: String = field.description.map(textToComment).getOrElse("")

  def isOption: Boolean = !field.required || field.default.nonEmpty

  def typeName: String = if (isOption) s"Option[${dataType.name}]" else dataType.name

  def play2JsonReader: String = {
    import ScalaDataType._
    def typeName = dataType.name
    def reads = if (isOption) {
      s"""({json: play.api.libs.json.JsValue =>
  json match {
    case play.api.libs.json.JsNull => None
    case _: play.api.libs.json.JsUndefined => None
    case _ => Some {
${dataType.play2JsonReader.indent(6)}
    }
  }
})"""
    } else {
      s"""({ json: play.api.libs.json.JsValue =>
${dataType.play2JsonReader.indent}
})"""
    }
    s"""val $name = $reads(json \\ "$originalName")"""
  }

  def play2JsonWriter: String = {
    import ScalaDataType._
    def writer = if (isOption) {
      s"""{ value: ${typeName} =>
  value.map { value: ${dataType.name} =>
${dataType.play2JsonWriter.indent(4)}
  }
}"""
    } else {
      s"""{ value: ${typeName} =>
${dataType.play2JsonWriter.indent}
}"""
    }
    s""""$originalName" -> (${writer})(value.$name)"""
  }

  override def src: String = {
    val decl = s"$description$name: $typeName"
    if (isOption) decl + " = None" else decl
  }
}

case class ScalaResponse(response: Response, packageName: String) {
  def code = response.code

  def typeName = s"Status$code"

  def dataType = ScalaDataType(response.dataType, None, packageName + "." + typeName)
}

sealed abstract class ScalaDataType(dataType: Datatype) extends Source {
  def name: String

  override def src = name

  def play2JsonReader: String

  def play2JsonWriter: String
}

object ScalaDataType {

  abstract class Basic(override val name: String, dataType: Datatype)
  extends ScalaDataType(dataType) {
    override def src = name
  }

  abstract class Formatted(name: String,
                           val format: Format,
                           dataType: Datatype)
  extends Basic(name, dataType)

  case object String extends Basic("String", Datatype.String) {
    def play2JsonReader: String = "json.as[String]"

    def play2JsonWriter: String = "play.api.libs.json.JsString(value)"
  }

  case object Int extends Basic("Int", Datatype.Integer) {
    def play2JsonReader: String = "json.as[Int]"

    def play2JsonWriter: String = "play.api.libs.json.JsNumber(value)"
  }

  case object Long extends Basic("Long", Datatype.Long) {
    def play2JsonReader: String = "json.as[Long]"

    def play2JsonWriter: String = "play.api.libs.json.JsNumber(value)"
  }

  case object Boolean extends Basic("Boolean", Datatype.Boolean) {
    def play2JsonReader: String = "json.as[Boolean]"

    def play2JsonWriter: String = "play.api.libs.json.JsBoolean(value)"
  }

  case object BigDecimal extends Basic("BigDecimal", Datatype.Decimal) {
    def play2JsonReader: String = "json.as[BigDecimal]"

    def play2JsonWriter: String = "play.api.libs.json.JsNumber(value)"
  }

  case object Uuid extends Formatted("java.util.UUID",
                                     Format.Uuid,
                                     Datatype.String) {
    def play2JsonReader: String = "java.util.UUID.fromString(json.as[String])"

    def play2JsonWriter: String = "play.api.libs.json.JsString(value.toString)"
  }

  case object DateTime extends Formatted("org.joda.time.DateTime",
                                         Format.DateTime,
                                         Datatype.String) {
    def play2JsonReader: String = {
      "org.joda.time.format.ISODateTimeFormat.basicDateTime.parseDateTime(json.as[String])"
    }

    def play2JsonWriter: String = {
      "play.api.libs.json.JsString(org.joda.time.format.ISODateTimeFormat.basicDateTime.print(value))"
    }
  }

  case class List(dataType: Datatype.List,
                  valueType: ScalaDataType)
  extends Basic(s"List[${valueType.name}]", dataType) {
    override def play2JsonReader: String = {
      val valueReader = valueType.play2JsonReader
      s"""json.as[play.api.libs.json.JsArray].value.map { json =>
${valueReader.indent}
}.toList"""
    }

    override def play2JsonWriter: String = {
      val valueWriter = valueType.play2JsonWriter
      s"""play.api.libs.json.Json.toJson(value.toSeq.map { value =>
${valueWriter.indent}
})"""
    }
  }

  case class UserType(dataType: Datatype.UserType, packageName: String)
  extends ScalaDataType(dataType) {
    override def name: String = packageName

    def resource = new ScalaResource(dataType.resource, packageName)

    def fields = dataType.fields.map { field =>
      new ScalaField(field, packageName)
    }

    def isPartial = fields.size != resource.fields.size

    override def play2JsonReader = {
      def fieldReader: String = {
        fields.map { field =>
          field.play2JsonReader.indent
        }.mkString("\n")
      }
      val topLevel: String = "(\\w+)\\.".r.findFirstMatchIn(packageName)
        .map(_.group(1)).getOrElse(packageName)
      if (isPartial) {
        s"""new $topLevel.ModelView[$topLevel.${resource.name}] {
  override val _self = (json \\ "_self").asOpt[String].map(new java.net.URL(_))
$fieldReader
}"""
      } else {
        s"""new $topLevel.Model {
  override val _self = (json \\ "_self").asOpt[String].map(new java.net.URL(_))
$fieldReader
}"""
      }
    }

    override def play2JsonWriter = {
      def fieldWriter: String = {
        fields.map { field =>
          field.play2JsonWriter.indent
        }.mkString(",\n").indent
      }
      s"""play.api.libs.json.Json.obj(
$fieldWriter
)"""
    }
  }

  class Reference(dataType: Datatype.Reference, packageName: String)
  extends UserType(dataType, packageName) {
    def field = new ScalaField(dataType.field, packageName)
  }

  def apply(dataType: Datatype, format: Option[Format], packageName: String): ScalaDataType = {
    dataType match {
      case Datatype.String => format.map {
        case Format.Uuid => Uuid
        case Format.DateTime => DateTime
      }.getOrElse {
        String
      }
      case Datatype.Integer => Int
      case Datatype.Long => Long
      case Datatype.Boolean => Boolean
      case Datatype.Decimal => BigDecimal
      case list: Datatype.List =>
        new List(list, apply(list.valueType, format, packageName))
      case r: Datatype.Reference => new Reference(r, packageName)
      case ut: Datatype.UserType => new UserType(ut, packageName)
    }
  }
}
