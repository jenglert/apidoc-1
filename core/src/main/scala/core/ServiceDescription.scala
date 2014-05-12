package core

import play.api.libs.json._
import java.util.UUID
import org.joda.time.format.ISODateTimeFormat

object ServiceDescription {

  def apply(apiJson: String): ServiceDescription = {
    val jsValue = Json.parse(apiJson)
    ServiceDescription(jsValue)
  }

  def apply(json: JsValue): ServiceDescription = {
    val internal = InternalServiceDescription(json)
    ServiceDescription(internal)
  }

}

private case class CanonicalField(resourceName: String, fieldName: String, datatype: Datatype)

case class ServiceDescription(internal: InternalServiceDescription) {

  private val canonicalFields: Seq[CanonicalField] = internal.resources.flatMap { ir =>
    ir.fields.filter { !_.datatype.isEmpty }.map { f =>
      CanonicalField(ir.name, f.name.get,
                     Datatype.findByName(f.datatype.get).getOrElse {
                       sys.error(s"Invalid datatype[${f.datatype}]")
                     })
    }
  }

  lazy val resources = internal.resources.map { Resource(canonicalFields, _) }
  lazy val baseUrl = internal.baseUrl.getOrElse { sys.error("Missing base_url") }
  lazy val name = internal.name.getOrElse { sys.error("Missing name") }
  lazy val basePath = internal.basePath
  lazy val description = internal.description

}

case class Resource(name: String,
                    path: String,
                    description: Option[String],
                    fields: Seq[Field],
                    operations: Seq[Operation])

case class Operation(method: String,
                     path: Option[String],
                     description: Option[String],
                     parameters: Seq[Field],
                     responses: Seq[Response])

object Operation {

  def apply(canonicalFields: Seq[CanonicalField], internal: InternalOperation): Operation = {
    val namedParameters = internal.namedParameters.map { paramName =>
      canonicalFields.find { _.fieldName == paramName }.getOrElse {
        sys.error(s"Could not find field definition for parameter named[$paramName]")
      }
    }

    Operation(method = internal.method.getOrElse { sys.error("Missing method") },
              path = internal.path,
              description = internal.description,
              parameters = internal.parameters.map { Field(canonicalFields, _) },
              responses = internal.responses.map { Response(_) })
  }

}

case class Field(name: String,
                 datatype: Datatype,
                 description: Option[String] = None,
                 required: Boolean = true,
                 multiple: Boolean = false,
                 references: Option[Reference] = None,
                 default: Option[String] = None,
                 example: Option[String] = None,
                 minimum: Option[Long] = None,
                 maximum: Option[Long] = None)

case class Reference(resource: String, field: String) {

  lazy val label = s"$resource.$field"

}

// TODO: Rename resource to datatype
case class Response(code: Int,
                    resource: String,
                    multiple: Boolean = false)

object Resource {

  def apply(canonicalFields: Seq[CanonicalField], ir: InternalResource): Resource = {
    val resourceFields = canonicalFields.filter { _.resourceName == ir.name }
    Resource(name = ir.name,
             path = ir.path,
             description = ir.description,
             fields = ir.fields.map { Field(canonicalFields, _) },
             operations = ir.operations.map { Operation(resourceFields, _) })
  }

}

object Response {

  def apply(ir: InternalResponse): Response = {
    val wd = WrappedDatatype(ir.datatype.get)
    Response(code = ir.code.toInt,
             resource = wd.datatype.name,
             multiple = wd.multiple)
  }

}

case class WrappedDatatype(datatype: Datatype, multiple: Boolean)

object WrappedDatatype {

  private val ArrayRx = """^\[(.+)\]$""".r

  def apply(value: String): WrappedDatatype = {
    // TODO: Parse ir.datatype properly
    //val multiple = ArrayRx.matches(ir.datatype)
    val datatype = Datatype.findByName(value).getOrElse {
      sys.error(s"Invalid datatype[${value}]")
    }
    WrappedDatatype(datatype = datatype, multiple = false)
  }

}

sealed abstract class Datatype(val name: String, val example: Option[String] = None, val description: Option[String] = None) {

  /**
   * Returns true if the string can be converted into an instance of this
   * datatype. False otherwise.
   */
  // TODO: def isValid(value: String): Boolean = validate(value).isEmpty

  /**
   * If the provided value is valid for this datatype - returns None.
   * Otherwise, returns a validation message.
   */
    // TODO: def validate(value: String): Option[String]

}

object Datatype {

  case object Boolean extends Datatype("boolean")

  case object Decimal extends Datatype("decimal")
  case object Integer extends Datatype("integer")
  case object Long extends Datatype("long")
  case object String extends Datatype("string")

  case object Uuid extends Datatype(name = "uuid",
                                    example = Some("5ecf6502-e532-4738-aad5-7ac9701251dd"),
                                    description = Some("String representation of a universally unique identifier (UUID)"))

  case object DateTimeIso8601 extends Datatype(name = "date-time-iso-8601",
                                               example = Some("2014-04-29T11:56:52Z"),
                                               description = Some("Date time format in ISO 8601"))

  case object Unit extends Datatype("unit")
  // TODO: case object Object extends Datatype("objects")

  val All = Seq(Boolean, Decimal, Integer, Long, String, Uuid, DateTimeIso8601, Unit)

  def findByName(name: String): Option[Datatype] = {
    All.find { dt => dt.name == name }
  }

}

object Field {

  def apply(canonicalFields: Seq[CanonicalField], internal: InternalField): Field = {
    canonicalFields.foreach { f => println(f) }

    val datatype = internal.datatype match {
      case Some(t: String) => {
        Datatype.findByName(t).getOrElse {
          sys.error(s"Invalid datatype[${t}]")
        }
      }

      case None => {
        val ref = internal.references.getOrElse {
          sys.error("Missing datatype and/or reference for field: " + internal)
        }

         val referencedField = canonicalFields.find { cf => cf.resourceName == ref.resource.get && cf.fieldName == ref.field.get }.getOrElse {
          sys.error(s"Reference not found[${ref.label}]. Fields: " + canonicalFields.map(cf => s"${cf.resourceName}.${cf.fieldName}").mkString(" "))
        }

        referencedField.datatype
      }
    }

    internal.default.map { v => assertValidDefault(datatype, v) }

    Field(name = internal.name.get,
          datatype = datatype,
          description = internal.description,
          references = internal.references.map { Reference(_) },
          required = internal.required,
          multiple = internal.multiple,
          default = internal.default,
          minimum = internal.minimum.map(_.toLong),
          maximum = internal.maximum.map(_.toLong),
          example = internal.example)
  }


  private val BooleanValues = Seq("true", "false")

  private def assertValidDefault(datatype: Datatype, value: String) {
    datatype match {
      case Datatype.Boolean => {
        if (!BooleanValues.contains(value)) {
          sys.error(s"Invalid value[${value}] for boolean. Must be one of: ${BooleanValues.mkString(" ")}")
        }
      }

      case Datatype.Integer => {
        value.toInt
      }

      case Datatype.Long => {
        value.toLong
      }

      case Datatype.Decimal => {
        BigDecimal(value)
      }

      case Datatype.Unit => {
        value == ""
      }

      case Datatype.Uuid => {
        UUID.fromString(value)
      }

      case Datatype.DateTimeIso8601 => {
        ISODateTimeFormat.basicDateTime.parseDateTime(value)
      }

      case Datatype.String => ()

    }
  }

}

object Reference {

  def apply(internal: InternalReference): Reference = {
    Reference(internal.resource.get, internal.field.get)
  }

}
