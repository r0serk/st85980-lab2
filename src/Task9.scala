package lab2

import Testing.check

/** 9. uzdevums: funkcija, kas pieņem kolonnas numuru (`Int`) un atgriež
  * augstāko vērtību šajā kolonnā.
  *
  * Mērķis: atrast lielāko dienas izmaiņu jebkurā indeksā. Kolonnu numuri
  * sākas ar 1 tāpat kā 3. uzdevuma izdrukā (`1. date`, `2. ...`).
  * Tukšas un neskaitliskas vērtības netiek ņemtas vērā.
  */
object Task9 extends LabTask {

  val number: Int   = 9
  val title: String = "Highest value in a column (by number)"

  /** Atrod augstāko vērtību kolonnā.
    *
    * @param frame        datu rāmis
    * @param columnNumber kolonnas numurs no 1 līdz kolonnu skaitam
    * @return `Right(augstākā vērtība)` vai `Left`, ja datu rāmī nav kolonnu,
    *         numurs ir ārpus robežām vai kolonnā nav neviena skaitļa
    *
    * Sarežģītība: O(n), kur n ir rindu skaits.
    */
  def maxInColumn(frame: DataFrame, columnNumber: Int): Either[String, Double] =
    if (frame.columnCount == 0) Left("the data frame has no columns")
    else if (columnNumber < 1 || columnNumber > frame.columnCount)
      Left(s"column number must be from 1 to ${frame.columnCount}, got $columnNumber")
    else {
      val index  = columnNumber - 1
      val values = frame.numbers(index).flatten
      if (values.isEmpty) Left(s"column $columnNumber '${frame.columns(index)}' has no numeric values")
      else Right(values.reduce((a, b) => math.max(a, b)))
    }

  /** Pārbauda lietotāja ievadi un atrod augstāko vērtību.
    *
    * @param frame datu rāmis
    * @param raw   lietotāja ievadītais teksts
    * @return `Right((numurs, vērtība))` vai `Left` ar kļūdas aprakstu
    */
  def maxFromInput(frame: DataFrame, raw: String): Either[String, (Int, Double)] =
    for {
      columnNumber <- Input.parseInt(raw)
      value        <- maxInColumn(frame, columnNumber)
    } yield (columnNumber, value)

  private val negatives = DataFrame(Vector("x"), Vector(Vector("-3"), Vector("-1"), Vector("-2")))

  def tests: Vector[TestResult] = Vector(
    check("Column 2 (ISE)", "0.01, -0.02, 0.03, 0.00, 0.02", Right(0.03), maxInColumn(Samples.frame, 2)),
    check("Column 3 (DAX) with NA", "0.02, 0.01, -0.04, NA, 0.03", Right(0.03), maxInColumn(Samples.frame, 3)),
    check("Column 5 (NIKKEI) with an empty cell", "-0.01, 0.05, 0, \"\", 0.02", Right(0.05), maxInColumn(Samples.frame, 5)),
    check("Only negative numbers", "-3, -1, -2", Right(-1.0), maxInColumn(negatives, 1)),
    check(
      "Column without numbers",
      "column 1 (date)",
      Left("column 1 'date' has no numeric values"),
      maxInColumn(Samples.frame, 1)
    ),
    check("Column number 0", "0", Left("column number must be from 1 to 6, got 0"), maxInColumn(Samples.frame, 0)),
    check("Negative column number", "-1", Left("column number must be from 1 to 6, got -1"), maxInColumn(Samples.frame, -1)),
    check("Column number too big", "7", Left("column number must be from 1 to 6, got 7"), maxInColumn(Samples.frame, 7)),
    check(
      "Largest Int",
      Int.MaxValue.toString,
      Left(s"column number must be from 1 to 6, got ${Int.MaxValue}"),
      maxInColumn(Samples.frame, Int.MaxValue)
    ),
    check(
      "Data frame without rows",
      "column 1, 0 rows",
      Left("column 1 'date' has no numeric values"),
      maxInColumn(Samples.frame.head(0), 1)
    ),
    check("Data frame without columns", "column 1", Left("the data frame has no columns"),
      maxInColumn(DataFrame(Vector.empty, Vector.empty), 1)),
    check("Input: number as text", "\" 2 \"", Right((2, 0.03)), maxFromInput(Samples.frame, " 2 ")),
    check(
      "Input: letters instead of a number",
      "abc",
      Left("'abc' is not a number (letters are not allowed)"),
      maxFromInput(Samples.frame, "abc")
    ),
    check("Input: decimal number", "2.5", Left("'2.5' is not a whole number"), maxFromInput(Samples.frame, "2.5")),
    check("Input: empty", "\"\"", Left("empty input - a number was expected"), maxFromInput(Samples.frame, "")),
    check("Input: symbols", "#2", Left("'#2' is not a valid integer"), maxFromInput(Samples.frame, "#2")),
    check(
      "Input: number outside Int",
      "99999999999",
      Left("'99999999999' is outside the Int range (-2147483648 .. 2147483647)"),
      maxFromInput(Samples.frame, "99999999999")
    )
  )

  // Atrod datumu rindā, kurā ir atrastā vērtība (izdrukai).
  private def dateOf(frame: DataFrame, index: Int, value: Double): String = {
    val dateIndex = frame.columnIndex("date")
    val row       = frame.rows.find(row => DataFrame.toNumber(row(index)).contains(value))
    (dateIndex, row) match {
      case (Some(i), Some(found)) if i != index => s" (date: ${found(i)})"
      case _                                    => ""
    }
  }

  def run(data: Dataset): Unit = {
    val frame = data.frame
    println("Highest value in every column:")
    frame.columns.indices.foreach { index =>
      val text = maxInColumn(frame, index + 1) match {
        case Right(value) => Format.number(value) + dateOf(frame, index, value)
        case Left(error)  => s"- ($error)"
      }
      println(f"  ${index + 1}%2d. ${frame.columns(index)}%-18s $text")
    }
    println()

    @annotation.tailrec
    def ask(): Unit =
      Input.promptUntilValid(s"Enter a column number (1-${frame.columnCount}): ", raw => maxFromInput(frame, raw)) match {
        case None => ()
        case Some((columnNumber, value)) =>
          val name = frame.columns(columnNumber - 1)
          println(s"Highest value in column $columnNumber '$name': ${Format.number(value)}${dateOf(frame, columnNumber - 1, value)}")
          if (Input.askYesNo("Check another column? (y/n): ")) ask() else ()
      }

    ask()
  }
}
