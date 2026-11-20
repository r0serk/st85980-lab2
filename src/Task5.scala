package lab2

import java.time.LocalDate
import Testing.check

/** 5. uzdevums: kārtot datu rāmi dilstošā secībā pēc datuma.
  *
  * Mērķis: jaunākie dati ir pirmie. Datums failā ir teksts (`5-Jan-09`),
  * tāpēc to nevar kārtot kā tekstu: teksta secībā `9-Jan-09` ir aiz
  * `10-Jan-09`. Katrs datums vispirms tiek pārveidots par `LocalDate`
  * ([[Dates.parse]]). Rindas ar nekorektu datumu tiek liktas beigās.
  */
object Task5 extends LabTask {

  val number: Int   = 5
  val title: String = "Sort by date (descending)"

  /** Sakārto rindas pēc datuma no jaunākā uz vecāko.
    *
    * @param frame      datu rāmis
    * @param dateColumn datuma kolonnas nosaukums (pēc noklusējuma `date`)
    * @return sakārtots datu rāmis vai `Left`, ja kolonnas nav
    *
    * Sarežģītība: O(n log n).
    */
  def sortByDateDescending(frame: DataFrame, dateColumn: String = "date"): Either[String, DataFrame] =
    frame.requireColumn(dateColumn).map { index =>
      frame.sortRowsBy(row => Dates.parse(row(index)), Dates.ordering, descending = true)
    }

  /** Atrod vērtības, kuras nav korekts datums.
    *
    * @param frame      datu rāmis
    * @param dateColumn datuma kolonnas nosaukums
    * @return nekorektās vērtības vai `Left`, ja kolonnas nav
    */
  def invalidDates(frame: DataFrame, dateColumn: String = "date"): Either[String, Vector[String]] =
    Samples.column(frame, dateColumn).map(values => values.filter(value => Dates.parse(value).isEmpty))

  private def datesAfterSort(frame: DataFrame): Either[String, Vector[String]] =
    sortByDateDescending(frame).flatMap(sorted => Samples.column(sorted, "date"))

  private val mixedFormats = DataFrame(
    Vector("date"),
    Vector(Vector("2009-01-07"), Vector("5-Jan-09"), Vector("06.01.2009"))
  )

  private val sameDates = DataFrame(
    Vector("date", "id"),
    Vector(Vector("5-Jan-09", "a"), Vector("5-Jan-09", "b"), Vector("6-Jan-09", "c"))
  )

  def tests: Vector[TestResult] = Vector(
    check(
      "Sort the sample data frame",
      "9-Jan-09, 10-Jan-09, 5-Jan-09, 12-Feb-10, abc",
      Right(Vector("12-Feb-10", "10-Jan-09", "9-Jan-09", "5-Jan-09", "abc")),
      datesAfterSort(Samples.frame)
    ),
    check("Date is read as a date, not text", "9-Jan-09", Some(LocalDate.of(2009, 1, 9)), Dates.parse("9-Jan-09")),
    check("Month names are read correctly", "15-Sep-10", Some(LocalDate.of(2010, 9, 15)), Dates.parse("15-Sep-10")),
    check("Date that does not exist", "31-Feb-09", None, Dates.parse("31-Feb-09")),
    check("Text instead of a date", "abc", None, Dates.parse("abc")),
    check("Invalid dates are found", "sample data frame", Right(Vector("abc")), invalidDates(Samples.frame)),
    check(
      "Different date formats",
      "2009-01-07, 5-Jan-09, 06.01.2009",
      Right(Vector("2009-01-07", "06.01.2009", "5-Jan-09")),
      datesAfterSort(mixedFormats)
    ),
    check(
      "Equal dates keep their order",
      "5-Jan-09 (a), 5-Jan-09 (b), 6-Jan-09 (c)",
      Right(Vector("c", "a", "b")),
      sortByDateDescending(sameDates).flatMap(sorted => Samples.column(sorted, "id"))
    ),
    check(
      "Date column does not exist",
      "column 'Datum'",
      Left("column 'Datum' not found (columns: date, ISE, DAX, FTSE, NIKKEI, EM)"),
      sortByDateDescending(Samples.frame, "Datum").map(_.rowCount)
    ),
    check("Empty data frame", "0 rows", Right(Vector.empty[String]), datesAfterSort(Samples.frame.head(0))),
    check(
      "Original data frame is not changed",
      "first date before sorting",
      "9-Jan-09",
      Samples.frame.rows.head.head
    )
  )

  def run(data: Dataset): Unit =
    sortByDateDescending(data.frame) match {
      case Left(error) => println(s"Error: $error")
      case Right(sorted) =>
        println(sorted.show(5))
        invalidDates(data.frame).foreach { invalid =>
          println(s"Rows with an invalid date (placed at the end): ${invalid.size}")
        }
    }
}
