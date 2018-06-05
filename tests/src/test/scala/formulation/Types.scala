package formulation

import formulation.refined._
import java.time.{Instant, LocalDateTime}
import java.util.UUID

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.Positive

case class Generic[T](value: T)

object Generic {
  def codec[A](avroPart: Avro[A], default: Option[A] = None): Avro[Generic[A]] =
    record1("formulation", "Generic")(Generic.apply[A])("value" -> member(avroPart, _.value, default))
}

case class UserId(id: Int)

sealed trait BookingProcess { val disc: Int }

object BookingProcess {
  final case class DateSelected(disc: Int, datetime: LocalDateTime) extends BookingProcess
  final case class NotStarted(disc: Int) extends BookingProcess
  final case class Cancelled(disc: Int) extends BookingProcess

  implicit val dateDeselected: Avro[BookingProcess.DateSelected] =
    record2("formulation", "DateSelected")(BookingProcess.DateSelected.apply)(
      "disc" -> member(int.discriminator(1), _.disc),
      "datetime" -> member(localDateTime, _.datetime)
    )

  implicit val notStarted: Avro[BookingProcess.NotStarted] =
    record1("formulation", "NotStarted")(BookingProcess.NotStarted.apply)(
      "disc" -> member(int.discriminator(0), _.disc)
    )

  implicit val cancelled: Avro[BookingProcess.Cancelled] =
    record1("formulation", "Cancelled")(BookingProcess.Cancelled.apply)(
      "disc" -> member(int.discriminator(2), _.disc)
    )

  implicit val codec: Avro[BookingProcess] =
    (dateDeselected | notStarted | cancelled).as[BookingProcess]
}

sealed trait Event

object Event {
  case class Completed(at: Instant) extends Event
  case class Failed(at: Instant) extends Event
  case class Started(at: Instant) extends Event

  implicit val completed: Avro[Completed] = record1("event", "Completed")(Completed.apply)("at" -> member(instant, _.at))
  implicit val failed: Avro[Failed] = record1("event", "Failed")(Failed.apply)("at" -> member(instant, _.at))
  implicit val started: Avro[Started] = record1("event", "Started")(Started.apply)("at" -> member(instant, _.at))

  implicit val codec: Avro[Event] = (completed | failed | started).as[Event]
}

sealed trait Fault

object Fault {
  case class Error(id: Int, message: String) extends Fault
  case class Failure(id: Int, message: Map[String, String], recoverable: Boolean) extends Fault

  implicit val error: Avro[Error] = record2("fault", "Error")(Error.apply)(
    "id" -> member(int, _.id, aliases = Seq("identifier", "errorId")),
    "message" -> member(string, _.message)
  )

  implicit val failure: Avro[Failure] = record3("fault", "Failure")(Failure.apply)(
    "id" -> member(int, _.id),
    "message" -> member(map(string)(Attempt.success)(identity), _.message),
    "recoverable" -> member(bool, _.recoverable, documentation = Some("States if we can retry the action"))
  )

  implicit val codec: Avro[Fault] = (error | failure).as[Fault]
}

case class UserV1(userId: UserId, username: String, email: String, password: String)

object UserV1 {
  implicit val codec: Avro[UserV1] = record4("user", "User")(UserV1.apply)(
    "userId" -> member(int.imap(UserId.apply)(_.id), _.userId),
    "username" -> member(string, _.username),
    "email" -> member(string, _.email),
    "password" -> member(string, _.password)
  )
}

case class UserV2(
                   userId: UserId,
                   username: String,
                   email: String,
                   password: String,
                   age: Option[Int],
                   countries: List[String],
                   bookingProcess: BookingProcess,
                   money: BigDecimal
                 )

object UserV2 {
  implicit val codec: Avro[UserV2] = record8("user", "User")(UserV2.apply)(
    //compatibility spec: we added aliases
    "userId" -> member(int.imap(UserId.apply)(_.id), _.userId, aliases = Seq("id")),
    //compatibility spec: we added documentation, due to canonical parsing this should not break compatibility
    "username" -> member(string, _.username, documentation = Some("The username of the user")),
    "email" -> member(string, _.email),
    "password" -> member(string, _.password),
    "age" -> member(option(int), _.age, Some(None)),
    "countries" -> member(list(string), _.countries, Some(List("Holland"))),
    "bookingProcess" -> member(BookingProcess.codec, _.bookingProcess, Some(BookingProcess.Cancelled(2))),
    "money" -> member(bigDecimal(300, 300), _.money, Some(1000))
  )
}

case class UserV3(
                   userId: UserId, username: String, email: String, password: String, age: Int
                 )

object UserV3 {
  implicit val codec: Avro[UserV3] = record5("user", "User")(UserV3.apply)(
    "userId" -> member(int.imap(UserId.apply)(_.id), _.userId),
    "username" -> member(string, _.username),
    "email" -> member(string, _.email),
    "password" -> member(string, _.password),
    "age" -> member(int, _.age)
  )
}

case class UserV4(
                   userId: UserId,
                   username: String,
                   email: String,
                   password: String,
                   age: Option[Int],
                   countries: List[String],
                   bookingProcess: BookingProcess
                 )

object UserV4 {
  implicit val codec: Avro[UserV4] = record7("user", "User")(UserV4.apply)(
    "userId" -> member(int.imap(UserId.apply)(_.id), _.userId),
    "username" -> member(string, _.username),
    "email" -> member(string, _.email),
    "password" -> member(string, _.password),
    "age" -> member(option(int), _.age, Some(None)),
    "countries" -> member(list(string), _.countries, Some(List("Holland"))),
    "bookingProcess" -> member(BookingProcess.codec, _.bookingProcess, Some(BookingProcess.Cancelled(2)))
  )
}

case class PersonUnrefined(name: String, age: Int)

object PersonUnrefined {
  implicit val codec: Avro[PersonUnrefined] = record2("user", "Person")(PersonUnrefined.apply)(
    "name" -> member(string, _.name),
    "age" -> member(int, _.age)
  )
}

case class PersonRefined(name: String Refined NonEmpty, age: Int Refined Positive)

object PersonRefined {
  implicit val codec: Avro[PersonRefined] = record2("user", "Person")(PersonRefined.apply)(
    "name" -> member(string.refine[NonEmpty], _.name),
    "age" -> member(int.refine[Positive], _.age)
  )
}

sealed trait UserEventV1

object UserEventV1 {
  case class Registered(userId: UUID, username: String, email: String, password: String, timestamp: Instant) extends UserEventV1
  case class Activated(userId: UUID, timestamp: Instant) extends UserEventV1
  case class Deactivated(userId: UUID, timestamp: Instant) extends UserEventV1

  implicit val registered: Avro[Registered] = record5("user.event", "Registered")(Registered.apply)(
    "userId" -> member(uuid, _.userId),
    "username" -> member(string, _.username),
    "email" -> member(string, _.email),
    "password" -> member(string, _.password),
    "timestamp" -> member(instant, _.timestamp)
  )

  implicit val deactivated: Avro[Deactivated] = record2("user.event", "Deactivated")(Deactivated.apply)(
    "userId" -> member(uuid, _.userId),
    "timestamp" -> member(instant, _.timestamp)
  )

  implicit val activated: Avro[Activated] = record2("user.event", "Activated")(Activated.apply)(
    "userId" -> member(uuid, _.userId),
    "timestamp" -> member(instant, _.timestamp)
  )

  implicit val avro: Avro[UserEventV1] = (registered | deactivated | activated).as[UserEventV1]
}

sealed trait UserEventV2

object UserEventV2 {
  case class Registered(userId: UUID, username: String, emailAddress: String, timestamp: Instant) extends UserEventV2
  case class Activated(userId: UUID, payload: String, timestamp: Instant) extends UserEventV2

  implicit val registered: Avro[Registered] = record4("user.event", "Registered")(Registered.apply)(
    "userId" -> member(uuid, _.userId),
    "username" -> member(string, _.username),
    "email" -> member(string, _.emailAddress),
    "timestamp" -> member(instant, _.timestamp)
  )

  implicit val activated: Avro[Activated] = record3("user.event", "Activated")(Activated.apply)(
    "userId" -> member(uuid, _.userId),
    "payload" -> member(string, _.payload, defaultValue = Some("some-payload")),
    "timestamp" -> member(instant, _.timestamp)
  )

  implicit val avro: Avro[UserEventV2] = (registered | activated).as[UserEventV2]
}

sealed trait ADTWithCaseObject
object ADTWithCaseObject {
  case class Case1(arg: String) extends ADTWithCaseObject
  case class Case2(arg1: Int, arg2: String) extends ADTWithCaseObject
  case object CaseObject1 extends ADTWithCaseObject

  implicit val case1: Avro[Case1] = record1("ADTWithCaseObject", "Case1")(Case1.apply)(
    "arg" -> member(string, _.arg)
  )

  implicit val case2: Avro[Case2] = record2("ADTWithCaseObject", "Case2")(Case2.apply)(
    "arg1" -> member(int, _.arg1),
    "arg2" -> member(string, _.arg2)
  )

  implicit val caseObject1: Avro[CaseObject1.type] = record0("ADTWithCaseObject", "CaseObject1")(CaseObject1)

  implicit val codec: Avro[ADTWithCaseObject] = (case1 | case2 | caseObject1).as[ADTWithCaseObject]
}

case class SomeProtocolHandshake(sessionId: UUID, sha1Checksum: Array[Byte])

object SomeProtocolHandshake {
  def uuidToBytes: UUID => Array[Byte] = uuid => {
    val bb = java.nio.ByteBuffer.wrap(new Array[Byte](16))
    bb.putLong(uuid.getMostSignificantBits)
    bb.putLong(uuid.getLeastSignificantBits)
    bb.array()
  }

  def uuidFromBytes: Array[Byte] => UUID = raw => {
    val bb = java.nio.ByteBuffer.wrap(raw)
    val high = bb.getLong()
    val low = bb.getLong()
    new UUID(high, low)
  }

  implicit val codec: Avro[SomeProtocolHandshake] = record2("protocol", "SomeProtocolHandshake")(SomeProtocolHandshake.apply)(
    "sessionId" -> member(fixed("uuid", 16).imap(raw => uuidFromBytes(raw))(id => uuidToBytes(id)), _.sessionId),
    "sha1Checksum" -> member(fixed("SHA1", 20), _.sha1Checksum)
  )
}