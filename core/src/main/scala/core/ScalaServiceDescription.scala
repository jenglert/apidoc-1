package core

import Text._

object ScalaUtil {
  def textToComment(text: String) = {
    val lines = text.split("\n")
    lines.mkString("/**\n * ", "\n * ", "\n */\n")
  }

  // TODO we need to separate the concept of field and parameter,
  // since they have different semantics. otherwise, we are going
  // to end up with more nasty methods like this
  def fieldsToArgList(fields: Seq[ScalaField], isParams: Boolean) = {
    val srcs = if (isParams) {
      fields.map {
        case field if field.isReference =>
          val realType = field.dataType.asInstanceOf[ScalaDataType.Reference].field.dataType
          val typeName = if (field.isOption) s"Option[${realType.name}] = None" else realType.name
          s"${field.name}: $typeName"
        case field => field.src
      }
    } else {
      fields.map(_.src)
    }
    srcs.mkString("\n", ",\n", "\n").indent
  }
}

import ScalaUtil._

class ScalaServiceDescription(serviceDescription: ServiceDescription)
{
  def name = safeName(serviceDescription.name)

  def description = serviceDescription.description.map(textToComment).getOrElse("")

  def resources = serviceDescription.resources.map { resource =>
    new ScalaResource(resource, Set.empty)
  }
}

class ScalaResource(resource: Resource, includedFields: Set[String])
{
  def name = singular(underscoreToInitCap(resource.name))

  def path = resource.path

  def description = resource.description.map(textToComment).getOrElse("")

  def fields = {
    val included: Seq[Field] = if (includedFields.isEmpty) {
      resource.fields
    } else {
      resource.fields.filter(f => includedFields(f.name))
    }
    included.map { field =>
      new ScalaField(field)
    }
  }.sorted

  def argList = fieldsToArgList(fields, isParams = false)

  def operations = resource.operations.map { operation =>
    new ScalaOperation(operation)
  }
}

class ScalaOperation(operation: Operation)
{
  def method = operation.method

  def path = operation.path

  def description = operation.description.map(textToComment).getOrElse("")

  def parameters = operation.parameters.map { new ScalaField(_) }.sorted

  def name = method.toLowerCase + path.map(safeName).getOrElse("").capitalize

  def responseTypeName = name.capitalize

  def argList = fieldsToArgList(parameters, isParams = true)

  def responses = operation.responses.map(new ScalaResponse(_))
}

class ScalaField(field: Field) extends Source with Ordered[ScalaField] {
  def name: String = snakeToCamelCase(field.name)

  def originalName: String = field.name

  def dataType: ScalaDataType = ScalaDataType(field.dataType, field.format)

  def isReference = dataType.getClass == classOf[ScalaDataType.Reference]

  def description: String = field.description.map(textToComment).getOrElse("")

  def isOption: Boolean = !field.required || field.default.nonEmpty

  def typeName: String = if (isOption) s"Option[${dataType.name}]" else dataType.name

  override def src: String = {
    val decl = s"$description$name: $typeName"
    if (isOption) decl + " = None" else decl
  }

  // we just want to make sure that fields with defaults
  // always come after those without, so that argument lists will
  // be valid. otherwise, preserve existing order
  override def compare(that: ScalaField): Int = {
    if (isOption) {
      if (that.isOption) {
        0
      } else {
        1
      }
    } else if (that.isOption) {
      -1
    } else {
      0
    }
  }
}

case class ScalaResponse(response: Response) {
  def code = response.code

  def dataType = ScalaDataType(response.dataType, None)
}

sealed abstract class ScalaDataType(dataType: Datatype) extends Source {
  def name: String

  override def src = name
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

  case object String extends Basic("String", Datatype.String)
  case object Int extends Basic("Int", Datatype.Integer)
  case object Long extends Basic("Long", Datatype.Long)
  case object Boolean extends Basic("Boolean", Datatype.Boolean)
  case object BigDecimal extends Basic("BigDecimal", Datatype.Decimal)

  case object Uuid extends Formatted("UUID",
                                     Format.Uuid,
                                     Datatype.String)

  case object DateTime extends Formatted("DateTime",
                                         Format.DateTime,
                                         Datatype.String)

  class List(dataType: Datatype.List,
             val valueType: ScalaDataType)
  extends Basic(s"List[${valueType.name}]", dataType)

  class UserType(dataType: Datatype.UserType) extends ScalaDataType(dataType) {
    override def name: String = underscoreToInitCap(dataType.name)
  }

  class Reference(dataType: Datatype.Reference) extends ScalaDataType(dataType) {
    override def name: String = resource.name

    def resource = new ScalaResource(
      resource  = dataType.underlying.resource,
      includedFields = Set(dataType.underlying.fieldName)
    )

    def field = new ScalaField(dataType.underlying.field)
  }

  def apply(dataType: Datatype, format: Option[Format]): ScalaDataType = {
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
        new List(list, apply(list.valueType, format))
      case ut: Datatype.UserType => new UserType(ut)
      case r: Datatype.Reference => new Reference(r)
    }
  }
}
