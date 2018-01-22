package formulation

import cats.{Invariant, ~>}
import org.apache.avro.Schema
import org.apache.avro.generic.GenericRecord
import org.apache.avro.io.{BinaryDecoder, BinaryEncoder}
import org.codehaus.jackson.JsonNode
import shapeless.ops.coproduct.Align
import shapeless.{Coproduct, Generic}

import scala.util.Try

final case class Member[F[_], A, B] private (
    typeClass: F[A],
    getter: B => A,
    aliases: Seq[String],
    defaultValue: Option[JsonNode],
    documentation: Option[String]
) {
  def mapTypeClass[G[_]](f: F ~> G): Member[G, A, B] = copy(typeClass = f(typeClass))
}


trait Transformer[F[_], I, O] {
  def apply(fi: F[I]): F[O]
}

object Transformer {
  implicit def alignedCoproduct[F[_], I <: Coproduct, Repr <: Coproduct, O](implicit
                                                                            I: Invariant[F],
                                                                            G: Generic.Aux[O, Repr],
                                                                            TA: Align[Repr, I],
                                                                            FA: Align[I, Repr]): Transformer[F, I, O] = new Transformer[F, I, O] {
    override def apply(fi: F[I]): F[O] =
      I.imap(fi)(a => G.from(FA(a)))(b => TA(G.to(b)))
  }
}

final case class RecordName(namespace: String, name: String)

final case class AvroDecodeContext[A](entity: A, binaryDecoder: Option[BinaryDecoder])

final case class AvroEncodeContext[A](entity: A, binaryEncoder: Option[BinaryEncoder])

final case class AvroEncodeResult(usedSchema: Schema, payload: Array[Byte])

sealed trait AvroDecodeFailure

object AvroDecodeFailure {
  final case class Errors(record: GenericRecord, errors: List[AvroDecodeError]) extends AvroDecodeFailure
  final case class Exception(throwable: Throwable) extends AvroDecodeFailure
  final case object Noop extends AvroDecodeFailure
}

sealed trait Attempt[+A] { self =>
  def flatMap[B](f: A => Attempt[B]): Attempt[B] = self match {
    case Attempt.Success(value) => f(value)
    case Attempt.Exception(ex) => Attempt.Exception(ex)
    case Attempt.Error(err) => Attempt.Error(err)
  }

  def map[B](f: A => B): Attempt[B] = self match {
    case Attempt.Success(value) => Attempt.Success(f(value))
    case Attempt.Exception(ex) => Attempt.Exception(ex)
    case Attempt.Error(err) => Attempt.Error(err)
  }
}

object Attempt {
  final case class Success[A](value: A) extends Attempt[A]
  final case class Exception(error: Throwable) extends Attempt[Nothing] {
    override def equals(obj: scala.Any): Boolean = {
      obj match {
        case Exception(other) => other.getMessage == error.getMessage
        case _ => false
      }
    }
  }
  final case class Error(error: String) extends Attempt[Nothing]

  def exception(ex: Throwable): Attempt[Nothing] = Attempt.Exception(ex)
  def error(msg: String): Attempt[Nothing] = Attempt.Error(msg)
  def success[A](value: A): Attempt[A] = Attempt.Success(value)

  def fromTry[A](t: Try[A]): Attempt[A] = t match {
    case scala.util.Failure(err) => exception(err)
    case scala.util.Success(value) => success(value)
  }

  def fromEither[L, R](t: Either[String, R]): Attempt[R] = t match {
    case Left(err) => error(err)
    case Right(value) => success(value)
  }

  def fromOption[A](option: Option[A], ifEmpty: String): Attempt[A] = option match {
    case None => error(ifEmpty)
    case Some(value) => success(value)
  }
}

final case class AvroDecodeException(record: GenericRecord, errors: List[AvroDecodeError]) extends Throwable(s"Failed to decode a `${record.getSchema.getFullName}`")