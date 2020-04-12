package docman.backend

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.Date

import docman.backend.csv.CSVHelpers._
import docman.core.Document
import org.scalacheck.Arbitrary
import org.specs2.ScalaCheck

trait DocmanArbitraries {self: ScalaCheck =>
  import Arbitrary._

  implicit def localDateTimeArb: Arbitrary[LocalDateTime] = Arbitrary{
    arbitrary[Date].map(d => LocalDateTime.ofInstant(d.toInstant, ZoneId.systemDefault()))
  }

  implicit def localDateArb: Arbitrary[LocalDate] = Arbitrary{
    arbitrary[LocalDateTime].map(_.toLocalDate)
  }

  implicit def docGen: Arbitrary[Document] = Arbitrary(for{
    sender <- Arbitrary.arbitrary[Option[String]]
    subject <- Arbitrary.arbitrary[Option[String]]
    tags <- Arbitrary.arbitrary[Set[String]]
    date <- Arbitrary.arbitrary[Option[LocalDate]]
    pages <- Arbitrary.arbitrary[Option[Int]]
    lastMod <- Arbitrary.arbitrary[LocalDateTime]
    createdTime <- Arbitrary.arbitrary[LocalDateTime]
  } yield Document(sender, subject, tags, date, pages, lastMod, createdTime))

}
class CSVStoreTest extends org.specs2.Specification with ScalaCheck with DocmanArbitraries {

  override def is = s2"""CSVStore parseLine · makeLine == identity: $writeParse
                         CSVStore makeLine produces no newlines: $noNewlines
    """

  def writeParse = prop { (s: String, doc: Document) =>
    parseLine(makeLine(s,doc)) must beRight((s,doc))
  }

  def noNewlines = prop { (s: String, doc: Document) =>
    makeLine(s, doc) must contain("\n").not
  }
}
