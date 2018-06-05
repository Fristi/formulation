package formulation

import java.nio.charset.Charset
import java.time.Instant
import java.util.UUID

import org.apache.avro.{Conversions, LogicalTypes}
import org.codehaus.jackson.JsonNode
import org.codehaus.jackson.node._
import shapeless.CNil

/**
  * Print a JsonNode given a value (JSON encoder)
  * @tparam A
  */
trait AvroDefaultValuePrinter[A] {
  def print(value: A): JsonNode
}

object AvroDefaultValuePrinter {

  import scala.collection.JavaConverters._

  def create[A](f: A => JsonNode): AvroDefaultValuePrinter[A] = new AvroDefaultValuePrinter[A] {
    override def print(value: A): JsonNode = f(value)
  }

  def by[A, B](printer: AvroDefaultValuePrinter[A])(f: B => A): AvroDefaultValuePrinter[B] = new AvroDefaultValuePrinter[B] {
    override def print(value: B): JsonNode = printer.print(f(value))
  }

  implicit val interpreter: AvroAlgebra[AvroDefaultValuePrinter] = new AvroAlgebra[AvroDefaultValuePrinter] with AvroDefaultValuePrinterRecordN {

    override val int: AvroDefaultValuePrinter[Int] = create(new IntNode(_))
    override val string: AvroDefaultValuePrinter[String] = create(new TextNode(_))
    override val bool: AvroDefaultValuePrinter[Boolean] = create(BooleanNode.valueOf)
    override val float: AvroDefaultValuePrinter[Float] = create(v => new DoubleNode(v.toDouble))

    override def bigDecimal(scale: Int, precision: Int): AvroDefaultValuePrinter[BigDecimal] = by(byteArray) { decimal =>
      val decimalType = LogicalTypes.decimal(precision, scale)
      val decimalConversion = new Conversions.DecimalConversion

      decimalConversion.toBytes(decimal.setScale(scale).bigDecimal, null, decimalType).array()
    }

    override def fixed(name: String, size: Int, namespace: Option[String] = None): AvroDefaultValuePrinter[Array[Byte]] = byteArray

    override val byteArray: AvroDefaultValuePrinter[Array[Byte]] =
      create(bytes => new TextNode(new String(bytes, Charset.forName("ISO-8859-1"))))

    override val double: AvroDefaultValuePrinter[Double] = create(new DoubleNode(_))

    override val long: AvroDefaultValuePrinter[Long] = create(new LongNode(_))

    override val cnil: AvroDefaultValuePrinter[CNil] = create(_ => NullNode.instance)

    override val uuid: AvroDefaultValuePrinter[UUID] = by(string)(_.toString)

    override val instant: AvroDefaultValuePrinter[Instant] = by(long)(_.toEpochMilli)

    override def option[A](from: AvroDefaultValuePrinter[A]): AvroDefaultValuePrinter[Option[A]] = create {
      case Some(v) => from.print(v)
      case None => NullNode.instance
    }

    override def list[A](of: AvroDefaultValuePrinter[A]): AvroDefaultValuePrinter[List[A]] =
      create(xs => new ArrayNode(JsonNodeFactory.instance).addAll(xs.map(of.print).asJava))

    override def set[A](of: AvroDefaultValuePrinter[A]): AvroDefaultValuePrinter[Set[A]] = by(list(of))(_.toList)

    override def vector[A](of: AvroDefaultValuePrinter[A]): AvroDefaultValuePrinter[Vector[A]] = by(list(of))(_.toList)

    override def seq[A](of: AvroDefaultValuePrinter[A]): AvroDefaultValuePrinter[Seq[A]] = by(list(of))(_.toList)
    
    override def map[K, V](value: AvroDefaultValuePrinter[V])(mapKey: String => Attempt[K])(contramapKey: K => String): AvroDefaultValuePrinter[Map[K, V]] = create { mm =>
      new ObjectNode(JsonNodeFactory.instance).putAll(mm.map { case (k, v) => contramapKey(k).trim -> value.print(v) }.asJava)
    }

    override def pmap[A, B](fa: AvroDefaultValuePrinter[A])(f: A => Attempt[B])(g: B => A): AvroDefaultValuePrinter[B] = by(fa)(g)

    override def imap[A, B](fa: AvroDefaultValuePrinter[A])(f: A => B)(g: B => A): AvroDefaultValuePrinter[B] = by(fa)(g)

    override def or[A, B](fa: AvroDefaultValuePrinter[A], fb: AvroDefaultValuePrinter[B]): AvroDefaultValuePrinter[Either[A, B]] = create {
      case Left(left) => fa.print(left)
      case Right(right) => fb.print(right)
    }
  }
}


