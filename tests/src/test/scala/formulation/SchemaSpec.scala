package formulation

import org.scalatest.{Matchers, WordSpec}

class SchemaSpec extends WordSpec with Matchers  {

  "AvroSchema" should {
    "generate for Int" in { schemaString(int) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"int"}]}"""}
    "generate for Double" in { schemaString(double) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"double"}]}"""}
    "generate for Float" in { schemaString(float) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"float"}]}"""}
    "generate for String" in { schemaString(string) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"string"}]}"""}
    "generate for Bool" in { schemaString(bool) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"boolean"}]}"""}
    "generate for BigDecimal" in { schemaString(bigDecimal(300, 300)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"bytes","logicalType":"decimal","precision":300,"scale":300}}]}"""}
    "generate for Array[Byte]" in { schemaString(byteArray) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"bytes"}]}"""}
    "generate for Array[Byte] (fixed)" in { schemaString(fixed("uuid", 16)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"fixed","name":"uuid","namespace":"","size":16}}]}""" }
    "generate for Array[Byte] (fixed with namespace)" in { schemaString(fixed("uuid", 16, Some("name.space"))) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"fixed","name":"uuid","namespace":"name.space","size":16}}]}""" }
    "generate for Long" in { schemaString(long) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"long"}]}"""}
    "generate for UUID" in { schemaString(uuid) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"string","logicalType":"uuid"}}]}"""}
    "generate for Instant" in { schemaString(instant) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"long","logicalType":"timestamp-millis"}}]}"""}
    "generate for CNil" in { schemaString(cnil) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":"null"}]}"""}
    "generate for Option[Int]" in { schemaString(option(int)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":["null","int"]}]}"""}
    "generate for List[Int]" in { schemaString(list(int)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"array","items":"int"}}]}"""}
    "generate for Set[Int]" in { schemaString(set(int)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"array","items":"int"}}]}"""}
    "generate for Vector[Int]" in { schemaString(vector(int)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"array","items":"int"}}]}"""}
    "generate for Seq[Int]" in { schemaString(seq(int)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"array","items":"int"}}]}"""}
    "generate for Map[String, Int]" in { schemaString(map(int)(Attempt.success)(identity)) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":{"type":"map","values":"int"}}]}"""}
    "generate for ADT" in { schemaString(BookingProcess.codec) shouldBe """{"type":"record","name":"Generic","namespace":"formulation","doc":"","fields":[{"name":"value","type":[{"type":"record","name":"Cancelled","doc":"","fields":[{"name":"disc","type":"int"}]},{"type":"record","name":"DateSelected","doc":"","fields":[{"name":"disc","type":"int"},{"name":"datetime","type":"string"}]},{"type":"record","name":"NotStarted","doc":"","fields":[{"name":"disc","type":"int"}]},"null"]}]}"""}

    "generate aliases for members" in {
      schema[Fault.Error].toString() shouldBe """{"type":"record","name":"Error","namespace":"fault","doc":"","fields":[{"name":"id","type":"int","aliases":["identifier","errorId"]},{"name":"message","type":"string"}]}"""
    }

    "generate doc for members" in {
      schema[Fault.Failure].toString() shouldBe """{"type":"record","name":"Failure","namespace":"fault","doc":"","fields":[{"name":"id","type":"int"},{"name":"message","type":{"type":"map","values":"string"}},{"name":"recoverable","type":"boolean","doc":"States if we can retry the action"}]}"""
    }

    def schemaString[A](avroPart: Avro[A]): String =
      AvroSchema[Generic[A]](Generic.codec(avroPart)).schema.toString
  }
}
