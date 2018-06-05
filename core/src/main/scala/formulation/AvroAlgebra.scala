package formulation

import java.time.Instant
import java.util.UUID

import shapeless.CNil

trait AvroAlgebra[F[_]] extends AvroAlgebraRecordN[F] {
  val int: F[Int]
  val string: F[String]
  val bool: F[Boolean]
  val float: F[Float]
  def bigDecimal(scale: Int, precision: Int): F[BigDecimal]
  val byteArray: F[Array[Byte]]
  val double: F[Double]
  val long: F[Long]
  val cnil: F[CNil]

  def fixed(name: String, size: Int, namespace: Option[String] = None): F[Array[Byte]]

  val uuid: F[UUID]
  val instant: F[Instant]

  def option[A](from: F[A]): F[Option[A]]
  def list[A](of: F[A]): F[List[A]]
  def set[A](of: F[A]): F[Set[A]]
  def vector[A](of: F[A]): F[Vector[A]]
  def seq[A](of: F[A]): F[Seq[A]]
  def map[K, V](value: F[V])(mapKey: String => Attempt[K])(contramapKey: K => String): F[Map[K, V]]

  def pmap[A,B](fa: F[A])(f: A => Attempt[B])(g: B => A): F[B]
  def imap[A,B](fa: F[A])(f: A => B)(g: B => A): F[B]
  def or[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}