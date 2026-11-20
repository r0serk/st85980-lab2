package lab2

import Testing.check

/** 7. uzdevums: kārtot datu rāmi pēc augstākā NIKKEI indeksa.
  *
  * Mērķis: rindas ar lielāko NIKKEI vērtību ir pirmās. Vērtības tiek
  * salīdzinātas kā skaitļi, nevis kā teksts (tekstā `"0.05"` < `"0.1"`, bet
  * `"-0.01"` < `"-0.02"`). Tukšas un neskaitliskas vērtības tiek liktas beigās.
  */
object Task7 extends LabTask {

  val number: Int   = 7
  val title: String = "Sort by the highest NIKKEI"

  /** Sakārto rindas pēc kolonnas vērtības no lielākās uz mazāko.
    *
    * @param frame  datu rāmis
    * @param column kolonnas nosaukums (pēc noklusējuma `NIKKEI`)
    * @return sakārtots datu rāmis vai `Left`, ja kolonnas nav vai tajā nav
    *         neviena skaitļa
    *
    * Sarežģītība: O(n log n).
    */
  def sortByHighest(frame: DataFrame, column: String = "NIKKEI"): Either[String, DataFrame] =
    frame.requireColumn(column).flatMap { index =>
      if (frame.rowCount > 0 && frame.numbers(index).forall(_.isEmpty))
        Left(s"column '${frame.columns(index)}' has no numeric values")
      else
        Right(frame.sortRowsBy(row => DataFrame.toNumber(row(index)), Ordering.Double.TotalOrdering, descending = true))
    }

  private def columnAfterSort(frame: DataFrame, column: String): Either[String, Vector[String]] =
    sortByHighest(frame, column).flatMap(sorted => Samples.column(sorted, column))

  private val values = DataFrame(
    Vector("id", "x"),
    Vector(Vector("a", "-1e-3"), Vector("b", "0.002"), Vector("c", "-0.0005"), Vector("d", "abc"))
  )

  private val ties = DataFrame(
    Vector("id", "x"),
    Vector(Vector("a", "1"), Vector("b", "2"), Vector("c", "1"))
  )

  def tests: Vector[TestResult] = Vector(
    check(
      "Sort the sample data frame by NIKKEI",
      "-0.01, 0.05, 0, \"\", 0.02",
      Right(Vector("0.05", "0.02", "0", "-0.01", "")),
      columnAfterSort(Samples.frame, "NIKKEI")
    ),
    check(
      "Dates follow their rows",
      "sample data frame",
      Right(Vector("10-Jan-09", "abc", "5-Jan-09", "9-Jan-09", "12-Feb-10")),
      sortByHighest(Samples.frame).flatMap(sorted => Samples.column(sorted, "date"))
    ),
    check(
      "Numbers, not text: negative and scientific notation",
      "-1e-3, 0.002, -0.0005, abc",
      Right(Vector("0.002", "-0.0005", "-1e-3", "abc")),
      columnAfterSort(values, "x")
    ),
    check(
      "Equal values keep their order",
      "a=1, b=2, c=1",
      Right(Vector("b", "a", "c")),
      sortByHighest(ties, "x").flatMap(sorted => Samples.column(sorted, "id"))
    ),
    check(
      "Column without numbers",
      "date",
      Left("column 'date' has no numeric values"),
      sortByHighest(Samples.frame, "date").map(_.rowCount)
    ),
    check(
      "Column does not exist",
      "NIKKEI225",
      Left("column 'NIKKEI225' not found (columns: date, ISE, DAX, FTSE, NIKKEI, EM)"),
      sortByHighest(Samples.frame, "NIKKEI225").map(_.rowCount)
    ),
    check("Empty data frame", "0 rows", Right(0), sortByHighest(Samples.frame.head(0)).map(_.rowCount))
  )

  def run(data: Dataset): Unit =
    sortByHighest(data.frame) match {
      case Left(error) => println(s"Error: $error")
      case Right(sorted) => println(sorted.show(5))
    }
}
