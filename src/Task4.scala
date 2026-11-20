package lab2

import Testing.check

/** 4. uzdevums: izvadīt pirmās 5 datu rāmja rindas.
  *
  * Mērķis: ātri apskatīt datu kopas saturu un pārliecināties, ka tā ir
  * nolasīta pareizi.
  */
object Task4 extends LabTask {

  val number: Int   = 4
  val title: String = "Show the first 5 rows"

  /** Atgriež pirmās `n` rindas.
    *
    * @param frame datu rāmis
    * @param n     rindu skaits (pēc noklusējuma 5); ja rindu ir mazāk, tiek
    *              atgrieztas visas; negatīvs `n` tiek uzskatīts par 0
    * @return jauns datu rāmis ar tām pašām kolonnām un ne vairāk kā `n` rindām
    */
  def firstRows(frame: DataFrame, n: Int = 5): DataFrame = frame.head(n)

  private def dates(frame: DataFrame): Either[String, Vector[String]] = Samples.column(frame, "date")

  def tests: Vector[TestResult] = Vector(
    check(
      "First 5 rows of 5",
      "n = 5",
      Right(Vector("9-Jan-09", "10-Jan-09", "5-Jan-09", "12-Feb-10", "abc")),
      dates(firstRows(Samples.frame))
    ),
    check("First 2 rows", "n = 2", Right(Vector("9-Jan-09", "10-Jan-09")), dates(firstRows(Samples.frame, 2))),
    check("More rows asked than there are", "n = 100", 5, firstRows(Samples.frame, 100).rowCount),
    check("Zero rows", "n = 0", 0, firstRows(Samples.frame, 0).rowCount),
    check("Negative number of rows", "n = -3", 0, firstRows(Samples.frame, -3).rowCount),
    check("Empty data frame", "0 rows, n = 5", 0, firstRows(Samples.frame.head(0)).rowCount),
    check("Columns are not changed", "n = 2", Samples.columns, firstRows(Samples.frame, 2).columns)
  )

  def run(data: Dataset): Unit = {
    println(firstRows(data.frame).show(5))
    println(s"(the data set has ${data.frame.rowCount} rows)")
  }
}
