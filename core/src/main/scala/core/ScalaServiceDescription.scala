package core

import Text._

object ScalaUtil {
  def textToComment(text: String) = {
    val lines = text.split("\n")
    lines.mkString("/**\n * ", "\n * ", "\n */\n")
  }

  def fieldsToArgList(fields: Seq[ScalaField]) = {
    fields.map(_.src.indent).mkString("\n", ",\n", "\n")
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

  def argList = fieldsToArgList(fields)

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

  def argList = fieldsToArgList(parameters)

  def responses = operation.responses.map(new ScalaResponse(_))
}

class ScalaField(field: Field) extends Source with Ordered[ScalaField] {
  def name: String = snakeToCamelCase(field.name)

  def originalName: String = field.name

  def dataType: ScalaDataType = new ScalaDataType(field.dataType, field.format)

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

  def dataType = new ScalaDataType(response.dataType, None)
}

// TODO ScalaDataType should mirror Datatype with its union type structure
// so that we keep the information about references. This is important,
// because a reference in a parameter declaration should generate the type
// of the referenced field, whereas a reference in a field declaration needs
// to generate a type with a subset of the fields of the referenced resource.
class ScalaDataType(dataType: Datatype, format: Option[Format]) extends Source {
  import Datatype._
  import Format._
  def name: String = dataType match {
    case String => format.map {
      case Uuid => "UUID"
      case DateTime => "DateTime"
    }.getOrElse {
      "String"
    }
    case Integer => "Int"
    case Long => "Long"
    case Boolean => "Boolean"
    case Decimal => "BigDecimal"
    case List(_, dataType) =>
      val sdt = new ScalaDataType(dataType, format)
      s"List[${sdt.name}]"
    case UserType(name) => underscoreToInitCap(name)
    case r: Reference =>
      singular(underscoreToInitCap(r.resourceName))
  }

  override val src: String = dataType.name
}
