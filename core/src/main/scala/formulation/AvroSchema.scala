package formulation

import java.time.Instant
import java.util.UUID

import org.apache.avro.{LogicalTypes, Schema, SchemaBuilder}
import shapeless.CNil

import scala.annotation.implicitNotFound

/**
  * Type class which will generate a Schema when calling `generateSchema`
  * @tparam A The type of the value to generate Schema from
  */
@implicitNotFound(msg = "AvroSchema[${A}] not found, did you implicitly define Avro[${A}]?")
trait AvroSchema[A] {
  val schema: Schema
}

object AvroSchema {

  import scala.collection.JavaConverters._

  def create[A](s: Schema): AvroSchema[A] = new AvroSchema[A] {
    override val schema: Schema = s
  }

  def by[A, B](fa: AvroSchema[A])(f: B => A): AvroSchema[B] = new AvroSchema[B] {
    override val schema: Schema = fa.schema
  }

  implicit val interpreter: AvroAlgebra[AvroSchema] = new AvroAlgebra[AvroSchema] with AvroSchemaRecordN {

    override val int: AvroSchema[Int] = AvroSchema.create(Schema.create(Schema.Type.INT))

    override val string: AvroSchema[String] = AvroSchema.create(Schema.create(Schema.Type.STRING))

    override val bool: AvroSchema[Boolean] = AvroSchema.create(Schema.create(Schema.Type.BOOLEAN))

    override val float: AvroSchema[Float] = AvroSchema.create(Schema.create(Schema.Type.FLOAT))

    override val byteArray: AvroSchema[Array[Byte]] = AvroSchema.create(Schema.create(Schema.Type.BYTES))

    override val double: AvroSchema[Double] = AvroSchema.create(Schema.create(Schema.Type.DOUBLE))

    override val long: AvroSchema[Long] = AvroSchema.create(Schema.create(Schema.Type.LONG))

    override val cnil: AvroSchema[CNil] = AvroSchema.create(Schema.create(Schema.Type.NULL))

    override def fixed(name: String, size: Int, namespace: Option[String] = None): AvroSchema[Array[Byte]] =
      AvroSchema.create(SchemaBuilder.fixed(name).namespace(namespace.orNull).size(size))

    override val uuid: AvroSchema[UUID] = {
      val schema = Schema.create(Schema.Type.STRING)
      LogicalTypes.uuid().addToSchema(schema)
      AvroSchema.create(schema)
    }

    override val instant: AvroSchema[Instant] = {
      val schema = Schema.create(Schema.Type.LONG)
      LogicalTypes.timestampMillis().addToSchema(schema)
      AvroSchema.create(schema)
    }

    override def bigDecimal(scale: Int, precision: Int): AvroSchema[BigDecimal] = {
      val schema = Schema.create(Schema.Type.BYTES)
      LogicalTypes.decimal(precision, scale).addToSchema(schema)
      AvroSchema.create(schema)
    }

    override def imap[A, B](fa: AvroSchema[A])(f: A => B)(g: B => A): AvroSchema[B] = by(fa)(g)

    override def option[A](from: AvroSchema[A]): AvroSchema[Option[A]] = AvroSchema.create(Schema.createUnion(Schema.create(Schema.Type.NULL), from.schema))

    override def list[A](of: AvroSchema[A]): AvroSchema[List[A]] = AvroSchema.create(Schema.createArray(of.schema))

    override def pmap[A, B](fa: AvroSchema[A])(f: A => Attempt[B])(g: B => A): AvroSchema[B] = by(fa)(g)

    override def set[A](of: AvroSchema[A]): AvroSchema[Set[A]] = AvroSchema.create(Schema.createArray(of.schema))

    override def vector[A](of: AvroSchema[A]): AvroSchema[Vector[A]] = AvroSchema.create(Schema.createArray(of.schema))

    override def seq[A](of: AvroSchema[A]): AvroSchema[Seq[A]] = AvroSchema.create(Schema.createArray(of.schema))

    override def map[K, V](value: AvroSchema[V])(mapKey: String => Attempt[K])(contramapKey: K => String): AvroSchema[Map[K, V]] = AvroSchema.create(Schema.createMap(value.schema))

    override def or[A, B](fa: AvroSchema[A], fb: AvroSchema[B]): AvroSchema[Either[A, B]] = {
      import scala.util.{Failure, Success, Try}

      // union schemas can't contain other union schemas as a direct
      // child, so whenever we create a union, we need to check if our
      // children are unions

      // if they are, we just merge them into the union we're creating

      def schemasOf(schema: Schema): Seq[Schema] = Try(schema.getTypes /* throws an error if we're not a union */) match {
        case Success(s) => s.asScala
        case Failure(_) => Seq(schema)
      }

      def moveNullToHead(schemas: Seq[Schema]) = {
        val (nulls, withoutNull) = schemas.partition(_.getType == Schema.Type.NULL)
        withoutNull ++ nulls
      }

      val subschemas = List(fa.schema, fb.schema).flatMap(schemasOf)

      AvroSchema.create(Schema.createUnion(moveNullToHead(subschemas).asJava))
    }

  }

  implicit def apply[A](implicit A: Avro[A]): AvroSchema[A] = A.apply[AvroSchema]
}