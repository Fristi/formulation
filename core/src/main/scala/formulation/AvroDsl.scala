package formulation

import java.time._
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.Invariant
import shapeless.{:+:, CNil, Coproduct, Inl, Inr}

import scala.util.Try

trait Avro[A] {
  def apply[F[_] : AvroAlgebra]: F[A]
}

object Avro {
  def applyAlgebra[F[_], A](avro: Avro[A])(implicit F: AvroAlgebra[F]): F[A] = avro.apply[F]
}

trait AvroDsl extends AvroDslRecordN { self =>

  val int: Avro[Int] = new Avro[Int] {
    override def apply[F[_] : AvroAlgebra]: F[Int] = implicitly[AvroAlgebra[F]].int
  }

  val string: Avro[String] = new Avro[String] {
    override def apply[F[_] : AvroAlgebra]: F[String] = implicitly[AvroAlgebra[F]].string
  }

  val bool: Avro[Boolean] = new Avro[Boolean] {
    override def apply[F[_] : AvroAlgebra]: F[Boolean] = implicitly[AvroAlgebra[F]].bool
  }

  val long: Avro[Long] = new Avro[Long] {
    override def apply[F[_] : AvroAlgebra]: F[Long] = implicitly[AvroAlgebra[F]].long
  }

  val double: Avro[Double] = new Avro[Double] {
    override def apply[F[_] : AvroAlgebra]: F[Double] = implicitly[AvroAlgebra[F]].double
  }

  val float: Avro[Float] = new Avro[Float] {
    override def apply[F[_] : AvroAlgebra]: F[Float] = implicitly[AvroAlgebra[F]].float
  }

  val cnil: Avro[CNil] = new Avro[CNil] {
    override def apply[F[_] : AvroAlgebra]: F[CNil] = implicitly[AvroAlgebra[F]].cnil
  }

  val byteArray: Avro[Array[Byte]] = new Avro[Array[Byte]] {
    override def apply[F[_] : AvroAlgebra]: F[Array[Byte]] = implicitly[AvroAlgebra[F]].byteArray
  }

  def bigDecimal(scale: Int = 2, precision: Int = 8): Avro[BigDecimal] = new Avro[BigDecimal] {
    override def apply[F[_] : AvroAlgebra]: F[BigDecimal] = implicitly[AvroAlgebra[F]].bigDecimal(scale, precision)
  }

  def fixed(name: String, size: Int, namespace: Option[String] = None): Avro[Array[Byte]] = new Avro[Array[Byte]] {
    override def apply[F[_] : AvroAlgebra]: F[Array[Byte]] = implicitly[AvroAlgebra[F]].fixed(name, size, namespace)
  }

  val uuid: Avro[UUID] = new Avro[UUID] {
    override def apply[F[_] : AvroAlgebra]: F[UUID] = implicitly[AvroAlgebra[F]].uuid
  }

  val instant: Avro[Instant] = new Avro[Instant] {
    override def apply[F[_] : AvroAlgebra]: F[Instant] = implicitly[AvroAlgebra[F]].instant
  }

  val localDate: Avro[LocalDate] =
    string.pmap(str => Attempt.fromTry(Try(LocalDate.parse(str, DateTimeFormatter.ISO_LOCAL_DATE))))(_.format(DateTimeFormatter.ISO_LOCAL_DATE))

  val localDateTime: Avro[LocalDateTime] =
    string.pmap(str => Attempt.fromTry(Try(LocalDateTime.parse(str, DateTimeFormatter.ISO_LOCAL_DATE_TIME))))(_.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  val zonedDateTime: Avro[ZonedDateTime] =
    string.pmap(str => Attempt.fromTry(Try(ZonedDateTime.parse(str, DateTimeFormatter.ISO_ZONED_DATE_TIME))))(_.format(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  def imap[A, B](fa: Avro[A])(f: A => B)(g: B => A): Avro[B] = new Avro[B] {
    override def apply[F[_] : AvroAlgebra]: F[B] = implicitly[AvroAlgebra[F]].imap(fa.apply[F])(f)(g)
  }

  def pmap[A, B](fa: Avro[A])(f: A => Attempt[B])(g: B => A): Avro[B] = new Avro[B] {
    override def apply[F[_] : AvroAlgebra]: F[B] = implicitly[AvroAlgebra[F]].pmap(fa.apply[F])(f)(g)
  }

  def option[A](value: Avro[A]): Avro[Option[A]] = new Avro[Option[A]] {
    override def apply[F[_] : AvroAlgebra]: F[Option[A]] = implicitly[AvroAlgebra[F]].option(value.apply[F])
  }

  def list[A](of: Avro[A]): Avro[List[A]] = new Avro[List[A]] {
    override def apply[F[_] : AvroAlgebra]: F[List[A]] = implicitly[AvroAlgebra[F]].list(of.apply[F])
  }

  def set[A](of: Avro[A]): Avro[Set[A]] = new Avro[Set[A]] {
    override def apply[F[_] : AvroAlgebra]: F[Set[A]] = implicitly[AvroAlgebra[F]].set(of.apply[F])
  }

  def vector[A](of: Avro[A]): Avro[Vector[A]] = new Avro[Vector[A]] {
    override def apply[F[_] : AvroAlgebra]: F[Vector[A]] = implicitly[AvroAlgebra[F]].vector(of.apply[F])
  }

  def seq[A](of: Avro[A]): Avro[Seq[A]] = new Avro[Seq[A]] {
    override def apply[F[_] : AvroAlgebra]: F[Seq[A]] = implicitly[AvroAlgebra[F]].seq(of.apply[F])
  }

  def map[K, V](of: Avro[V])(mapKey: String => Attempt[K])(contramapKey: K => String): Avro[Map[K, V]] = new Avro[Map[K, V]] {
    override def apply[F[_] : AvroAlgebra]: F[Map[K, V]] = implicitly[AvroAlgebra[F]].map(of.apply[F])(mapKey)(contramapKey)
  }

  def or[A, B](fa: Avro[A], fb: Avro[B]): Avro[Either[A, B]] = new Avro[Either[A, B]] {
    override def apply[F[_] : AvroAlgebra]: F[Either[A, B]] = implicitly[AvroAlgebra[F]].or(fa.apply[F], fb.apply[F])
  }

  implicit class RichAvro[A](val fa: Avro[A]) {
    def imap[B](f: A => B)(g: B => A): Avro[B] = self.imap(fa)(f)(g)
    def pmap[B](f: A => Attempt[B])(g: B => A): Avro[B] = self.pmap(fa)(f)(g)
    def discriminator(v: A): Avro[A] =
      pmap(a => if(v == a) Attempt.success(v) else Attempt.error(s"Value '$a' didn't equal static('$v')"))(identity)
    def |[B](fb: Avro[B]): UnionBuilder[A :+: B :+: CNil] =
      new UnionBuilder[CNil](cnil).add(fb).add(fa)
  }

  implicit val invariantFunctor: Invariant[Avro] = new Invariant[Avro] {
    override def imap[A, B](fa: Avro[A])(f: A => B)(g: B => A): Avro[B] = self.imap(fa)(f)(g)
  }

  final class UnionBuilder[B <: Coproduct](fb: Avro[B]) {

    def add[A](fa: Avro[A]): UnionBuilder[A :+: B] = {
      val coproduct = imap(or(fa, fb)) {
        case Left(l) => Inl(l)
        case Right(r) => Inr(r)
      } {
        case Inl(l) => Left(l)
        case Inr(r) => Right(r)
      }

      new UnionBuilder(coproduct)
    }

    def |[A](fa: Avro[A]): UnionBuilder[A :+: B] = add(fa)

    def as[A](implicit T: Transformer[Avro, B, A]): Avro[A] = T(fb)
  }
}