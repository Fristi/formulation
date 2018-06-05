package formulation

import java.nio.ByteBuffer
import java.time.Instant
import java.util.UUID

import cats.Semigroupal
import cats.data.Validated
import org.apache.avro.{Conversions, LogicalTypes, Schema}
import org.apache.avro.util.Utf8
import shapeless.CNil

import scala.annotation.implicitNotFound
import scala.util.Try
import cats.implicits._
import org.apache.avro.generic.{GenericRecord, GenericFixed}

@implicitNotFound(msg = "AvroDecoder[${A}] not found, did you implicitly define Avro[${A}]?")
sealed trait AvroDecoder[A] { self =>
  def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], A]

  def map[B](f: A => B): AvroDecoder[B] = new AvroDecoder[B] {
    override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], B] =
      self.decode(path, schema, data).map(f)
  }

  def andThen[B](f: A => Either[Throwable, B]): AvroDecoder[B] = new AvroDecoder[B] {
    override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], B] =
      self.decode(path, schema, data).andThen(a => Validated.fromEither(f(a)).leftMap(ex => AvroDecodeError.Exception(path, ex) :: Nil))
  }
}

sealed trait AvroDecodeError

object AvroDecodeError {
  final case class NameMismatch(path: JsonPointer, expectedName: String, recordName: String, schemaName: String) extends AvroDecodeError
  final case class TypeMismatch(path: JsonPointer, actual: Any, schema: Schema) extends AvroDecodeError
  final case class Union(path: JsonPointer, errors: List[AvroDecodeError]) extends AvroDecodeError
  final case class Exception(path: JsonPointer, throwable: Throwable) extends AvroDecodeError
  final case class Error(path: JsonPointer, message: String) extends AvroDecodeError
  final case class SchemaDoesNotHaveField(path: JsonPointer, field: String, schema: Schema) extends AvroDecodeError

  def fromAttempt[A](path: JsonPointer, attempt: Attempt[A]): Either[AvroDecodeError, A] = attempt match {
    case Attempt.Exception(ex) => Left(Exception(path, ex))
    case Attempt.Error(err) => Left(Error(path, err))
    case Attempt.Success(value) => Right(value)
  }
}

sealed trait JsonPointerNode

object JsonPointerNode {
  final case class Member(name: String) extends JsonPointerNode
  final case class Index(index: Int) extends JsonPointerNode
}

final case class JsonPointer private(nodes: List[JsonPointerNode]) {
  def member(name: String): JsonPointer = copy(nodes = JsonPointerNode.Member(name) :: nodes)
  def index(idx: Int): JsonPointer = copy(nodes = JsonPointerNode.Index(idx) :: nodes)

  override def toString: String = nodes.foldLeft("") { case (acc, el) =>
    el match {
      case JsonPointerNode.Index(idx) => s"[$idx]" + acc
      case JsonPointerNode.Member(name) => "/" + name + acc
    }
  }
}

object JsonPointer {
  def apply(): JsonPointer = JsonPointer(List.empty)
}


object AvroDecoder {

  import scala.collection.JavaConverters._

  def partial[A](f: PartialFunction[(JsonPointer, Schema, Any), Validated[List[AvroDecodeError], A]]): AvroDecoder[A] = new AvroDecoder[A] {
    override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], A] =
      f.applyOrElse((path, schema, data), (a: (JsonPointer, Schema, Any)) => Validated.invalid(List(AvroDecodeError.TypeMismatch(path, a._2, schema))))
  }

  def record[A](namespace: String, name: String)(f: (JsonPointer, Schema, GenericRecord) => Validated[List[AvroDecodeError], A]): AvroDecoder[A] = new AvroDecoder[A] {
    override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], A] = data match {
      case record: GenericRecord =>
        if(record.getSchema.getFullName == namespace + "." + name && schema.getFullName == namespace + "." + name) {
          f(path, schema, record)
        } else {
          Validated.invalid(List(AvroDecodeError.NameMismatch(
            path, expectedName = namespace + "." + name, schemaName = schema.getFullName, recordName = record.getSchema.getFullName)))
        }
      case _ =>
        Validated.invalid(List(AvroDecodeError.TypeMismatch(path, data, schema)))
    }
  }

  def fail[A](error: String): AvroDecoder[A] = new AvroDecoder[A] {
    override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], A] = Validated.invalid(AvroDecodeError.Error(path, error) :: Nil)
  }

  implicit val interpreter: AvroAlgebra[AvroDecoder] = new AvroAlgebra[AvroDecoder] with AvroDecoderRecordN {

    override val int: AvroDecoder[Int] = partial { case (_, _, v: Int) => Validated.valid(v) }
    override val string: AvroDecoder[String] = partial { case (_, _, v: Utf8) => Validated.valid(v.toString) }
    override val bool: AvroDecoder[Boolean] = partial { case (_, _, v: Boolean) => Validated.valid(v) }
    override val float: AvroDecoder[Float] = partial { case (_, _, v: Float) => Validated.valid(v) }
    override val byteArray: AvroDecoder[Array[Byte]] = partial[Array[Byte]] { case (_, _, v: ByteBuffer) => Validated.valid(v.array()) }
    override val double: AvroDecoder[Double] = partial { case (_, _, v: Double) => Validated.valid(v) }
    override val long: AvroDecoder[Long] = partial { case (_, _, v: Long) => Validated.valid(v) }
    override val cnil: AvroDecoder[CNil] = fail("Unable to decode cnil")

    override val uuid: AvroDecoder[UUID] = string.andThen(str => Either.fromTry(Try(UUID.fromString(str))))

    override val instant: AvroDecoder[Instant] = long.andThen(ts => Either.fromTry(Try(Instant.ofEpochMilli(ts))))

    override def fixed(name: String, size: Int, namespace: Option[String] = None): AvroDecoder[Array[Byte]] =
      partial { case (_, s, v: GenericFixed) => Validated.valid(v.bytes()) }

    override def bigDecimal(scale: Int, precision: Int): AvroDecoder[BigDecimal] = partial[BigDecimal] { case (path, _, v: ByteBuffer) =>

      val decimalType = LogicalTypes.decimal(precision, scale)
      val decimalConversion = new Conversions.DecimalConversion

      Validated.fromTry(Try(decimalConversion.fromBytes(v, null, decimalType))).map(x => BigDecimal(x)).leftMap(ex => AvroDecodeError.Exception(path, ex) :: Nil)
    }

    override def imap[A, B](fa: AvroDecoder[A])(f: A => B)(g: B => A): AvroDecoder[B] = new AvroDecoder[B] {
      override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], B] = fa.decode(path, schema, data).map(f)
    }

    override def option[A](from: AvroDecoder[A]): AvroDecoder[Option[A]] = new AvroDecoder[Option[A]] {

      override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], Option[A]] = {
        data match {
          case null => Validated.valid(None)
          case _ =>
            schema.getTypes.asScala.toList.filterNot(_.getType == Schema.Type.NULL).headOption match {
              case Some(s) => from.decode(path, s, data).map(Some.apply)
              case None => Validated.invalid(AvroDecodeError.Error(path, "Non-null case not found, should be impossible") :: Nil)
            }
        }
      }
    }

    override def list[A](of: AvroDecoder[A]): AvroDecoder[List[A]] =
      partial {
        case (path, schema, x: java.util.Collection[_]) =>
          x.asScala.toList.zipWithIndex.traverse[Validated[List[AvroDecodeError], ?], A] { case (y, idx) => of.decode(path.index(idx), schema.getElementType, y) }
      }

    override def set[A](of: AvroDecoder[A]): AvroDecoder[Set[A]] =
      list(of).map(_.toSet)

    override def vector[A](of: AvroDecoder[A]): AvroDecoder[Vector[A]] =
      list(of).map(_.toVector)

    override def seq[A](of: AvroDecoder[A]): AvroDecoder[Seq[A]] =
      list(of).map(_.toSeq)

    override def map[K, V](of: AvroDecoder[V])(mapKey: String => Attempt[K])(contramapKey: K => String): AvroDecoder[Map[K, V]] =
      partial { case (path, schema, x: java.util.Map[_, _]) =>
        x.asScala
          .toMap
          .map { case (k, v) => k.toString -> v }
          .foldLeft(Validated.valid(Map.empty[K, V]) : Validated[List[AvroDecodeError], Map[K, V]]) { case (init, (key, value)) =>
            def k = Validated.fromEither(AvroDecodeError.fromAttempt(path.member(key), mapKey(key)).leftMap(_ :: Nil))
            def v = of.decode(path.member(key), schema.getValueType, value)

            Semigroupal.map3[Validated[List[AvroDecodeError], ?], K, V, Map[K, V], Map[K, V]](k, v, init) { case (k, v, acc) => acc + (k -> v) }
          }
      }

    override def pmap[A, B](fa: AvroDecoder[A])(f: A => Attempt[B])(g: B => A): AvroDecoder[B] = new AvroDecoder[B] {
      override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], B] = fa.decode(path, schema, data).andThen(a => Validated.fromEither(AvroDecodeError.fromAttempt(path, f(a)).leftMap(_ :: Nil)))
    }

    override def or[A, B](fa: AvroDecoder[A], fb: AvroDecoder[B]): AvroDecoder[Either[A, B]] = new AvroDecoder[Either[A, B]] {
      override def decode(path: JsonPointer, schema: Schema, data: Any): Validated[List[AvroDecodeError], Either[A, B]] = {

        def toDecodeError[Z](s: Schema, d: AvroDecoder[Z], data: Any) = d.decode(path, s, data)

        def loopValidated(schemas: List[Schema]): Validated[List[AvroDecodeError], Either[A, B]] = schemas match {
          case x :: xs => toDecodeError(x, fa, data).map(Left.apply).findValid(toDecodeError(x, fb, data).map(Right.apply)).findValid(loopValidated(xs))
          case Nil => Validated.invalid(List.empty)
        }

        schema.getType match {
          case Schema.Type.UNION => loopValidated(schema.getTypes.asScala.toList).leftMap(errs => AvroDecodeError.Union(path, errs) :: Nil)
          case _ => fa.decode(path, schema, data).map(Left.apply) findValid fb.decode(path, schema, data).map(Right.apply)
        }
      }
    }
  }

  implicit def apply[A](implicit A: Avro[A]): AvroDecoder[A] = A.apply[AvroDecoder]


}