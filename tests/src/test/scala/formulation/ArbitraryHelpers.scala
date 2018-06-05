package formulation

import java.time._
import java.util.UUID

import org.scalacheck.{Arbitrary, Gen}
import scala.collection.JavaConverters._

trait ArbitraryHelpers {

  val mapGen: Gen[Map[String, Int]] = {
    def genKv = for {
      key <- Gen.alphaLowerStr
      value <- Gen.choose(Int.MinValue, Int.MaxValue)
    } yield key -> value

    Gen.nonEmptyListOf(genKv).map(_.toMap)
  }

  implicit val zonedDateTimeArb: Arbitrary[ZonedDateTime] = Arbitrary(
    for {
      instant <- instantArb.arbitrary
      zoneId  <- Gen.oneOf(ZoneId.getAvailableZoneIds.asScala.toList.filter(_ != "GMT0")).map(ZoneId.of)
    } yield ZonedDateTime.ofInstant(instant, zoneId)
  )

  implicit val bookingProcessArb: Arbitrary[BookingProcess] = Arbitrary {
    def genNotStarted = Gen.const(BookingProcess.NotStarted(0))
    def genCancelled = Gen.const(BookingProcess.Cancelled(2))
    def genDateSelected = localDateTineArb.arbitrary.map(date => BookingProcess.DateSelected(1, date))

    Gen.oneOf(
      genNotStarted,
      genDateSelected,
      genCancelled
    )
  }

  implicit val faultArb: Arbitrary[Fault] = Arbitrary {
    def error = for {
      id <- Gen.choose(0, Int.MaxValue)
      message <- Gen.alphaStr
    } yield Fault.Error(id, message)

    def failure = for {
      id <- Gen.choose(0, Int.MaxValue)
      message <- Gen.mapOf(Gen.alphaStr.flatMap(a => Gen.alphaStr.map(b => a -> b)))
      recoverable <- Gen.oneOf(true, false)
    } yield Fault.Failure(id, message, recoverable)

    Gen.oneOf(error, failure)
  }

  implicit val eventArb: Arbitrary[Event] = Arbitrary {
    def completed = instantArb.arbitrary.map(Event.Completed)
    def started = instantArb.arbitrary.map(Event.Started)
    def failed = instantArb.arbitrary.map(Event.Failed)

    Gen.oneOf(
      completed,
      started,
      failed
    )
  }

  implicit val uuidArb: Arbitrary[UUID] = Arbitrary(Gen.uuid)
  implicit val localDateArb: Arbitrary[LocalDate] = Arbitrary {
    for {
      year <- Gen.choose(1970, 2050)
      month <- Gen.choose(1, 12)
      day <- Gen.choose(1, 28)
    } yield LocalDate.of(year, month, day)
  }

  implicit val instantArb: Arbitrary[Instant] = Arbitrary {
    Gen.choose(0, Long.MaxValue).map(ts => Instant.ofEpochMilli(ts))
  }

  implicit val localDateTineArb: Arbitrary[LocalDateTime] = Arbitrary {
    for {
      year <- Gen.choose(1970, 2050)
      month <- Gen.choose(1, 12)
      day <- Gen.choose(1, 28)
      hour <- Gen.choose(0, 23)
      minute <- Gen.choose(0, 59)
      second <- Gen.choose(0, 59)
    } yield LocalDateTime.of(LocalDate.of(year, month, day), LocalTime.of(hour, minute, second))
  }

  implicit val userV1Arb: Arbitrary[UserV1] = Arbitrary {
    for {
      userId <- Gen.choose(0, Int.MaxValue).map(UserId)
      username <- Gen.alphaStr
      email <- Gen.alphaStr
      password <- Gen.alphaStr
    } yield UserV1(userId, username, email, password)
  }

  implicit val userV2Arb: Arbitrary[UserV2] = Arbitrary {
    for {
      userId <- Gen.choose(0, Int.MaxValue).map(UserId)
      username <- Gen.alphaStr
      email <- Gen.alphaStr
      password <- Gen.alphaStr
      age <- Gen.option(Gen.choose(0, 99))
      countries <- Gen.listOf(Gen.alphaStr)
      bookingProcess <- bookingProcessArb.arbitrary
      money <- Gen.choose(0, Int.MaxValue).map(BigDecimal.apply)
    } yield UserV2(userId, username, email, password, age, countries, bookingProcess, money)
  }

  implicit val adtWithCaseObjectCase1Arb: Arbitrary[ADTWithCaseObject.Case1] = Arbitrary {
    for {
      arg <- Gen.alphaStr
    } yield ADTWithCaseObject.Case1(arg)
  }

  implicit val adtWithCaseObjectCase2Arb: Arbitrary[ADTWithCaseObject.Case2] = Arbitrary {
    for {
      arg1 <- Gen.choose(-100, 100)
      arg2 <- Gen.alphaStr
    } yield ADTWithCaseObject.Case2(arg1, arg2)
  }

  implicit val adtWithCaseObjectArb: Arbitrary[ADTWithCaseObject] = Arbitrary {
    def case1 = adtWithCaseObjectCase1Arb.arbitrary
    def case2 = adtWithCaseObjectCase2Arb.arbitrary

    Gen.oneOf(case1, case2, Gen.const(ADTWithCaseObject.CaseObject1))
  }

  implicit val someProtocolHandshakeArb: Arbitrary[SomeProtocolHandshake] = Arbitrary {
    for {
      sessionId <- Gen.uuid
      sha1Checksum <- Gen.const(java.security.MessageDigest.getInstance("SHA-1")
        .digest(sessionId.toString.getBytes("UTF-8")))
    } yield SomeProtocolHandshake(sessionId, sha1Checksum)
  }
}
