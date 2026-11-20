package lab2

import Testing.check

/** 6. uzdevums: noņemt EM kolonnu.
  *
  * Mērķis: izveidot datu rāmi bez jaunattīstības tirgu indeksa (EM).
  * Sākotnējais datu rāmis netiek mainīts: tiek atgriezts jauns datu rāmis.
  */
object Task6 extends LabTask {

  val number: Int   = 6
  val title: String = "Remove the EM column"

  /** Noņem kolonnu no datu rāmja.
    *
    * @param frame datu rāmis
    * @param name  noņemamās kolonnas nosaukums (pēc noklusējuma `EM`);
    *              lielie un mazie burti netiek ņemti vērā
    * @return jauns datu rāmis bez kolonnas vai `Left`, ja kolonnas nav
    */
  def removeColumn(frame: DataFrame, name: String = "EM"): Either[String, DataFrame] =
    frame.dropColumn(name)

  private val removed: Either[String, DataFrame] = removeColumn(Samples.frame)

  def tests: Vector[TestResult] = Vector(
    check(
      "Columns after removing EM",
      "date, ISE, DAX, FTSE, NIKKEI, EM",
      Right(Vector("date", "ISE", "DAX", "FTSE", "NIKKEI")),
      removed.map(_.columns)
    ),
    check("Every row has 5 values", "5 rows", Right(Vector(5, 5, 5, 5, 5)), removed.map(_.rows.map(_.size))),
    check(
      "Other values are not changed",
      "NIKKEI column",
      Samples.column(Samples.frame, "NIKKEI"),
      removed.flatMap(frame => Samples.column(frame, "NIKKEI"))
    ),
    check(
      "Name in lower case",
      "em",
      Right(Vector("date", "ISE", "DAX", "FTSE", "NIKKEI")),
      removeColumn(Samples.frame, "em").map(_.columns)
    ),
    check(
      "Column does not exist",
      "XYZ",
      Left("column 'XYZ' not found (columns: date, ISE, DAX, FTSE, NIKKEI, EM)"),
      removeColumn(Samples.frame, "XYZ").map(_.columns)
    ),
    check(
      "Removing EM twice",
      "data frame without EM",
      Left("column 'EM' not found (columns: date, ISE, DAX, FTSE, NIKKEI)"),
      removed.flatMap(frame => removeColumn(frame)).map(_.columns)
    ),
    check("Original data frame still has EM", "Samples.frame", true, Samples.frame.columns.contains("EM"))
  )

  def run(data: Dataset): Unit =
    removeColumn(data.frame) match {
      case Left(error) => println(s"Error: $error")
      case Right(result) =>
        println(s"Columns before: ${data.frame.columns.mkString(", ")}")
        println(s"Columns after:  ${result.columns.mkString(", ")}")
        println()
        println(result.show(5))
    }
}
