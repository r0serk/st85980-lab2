package lab2

import Testing.check

/** 8. uzdevums: izveidot jaunu kolonnu ar DAX un FTSE vidējo vērtību.
  *
  * Mērķis: katrai dienai aprēķināt Vācijas (DAX) un Lielbritānijas (FTSE)
  * indeksu izmaiņu vidējo vērtību `(DAX + FTSE) / 2`. Ja kādā rindā vienas
  * no vērtībām nav vai tā nav skaitlis, vidējā vērtība nav aprēķināma un
  * šūna paliek tukša (tā netiek aizstāta ar 0, lai nesagrozītu datus).
  */
object Task8 extends LabTask {

  val number: Int   = 8
  val title: String = "Add the DAX and FTSE average column"

  /** Jaunās kolonnas nosaukums. */
  val averageColumn: String = "DAX_FTSE_AVG"

  /** Aprēķina divu šūnu vidējo vērtību.
    *
    * @param first  pirmā vērtība (teksts)
    * @param second otrā vērtība (teksts)
    * @return vidējā vērtība kā teksts vai `""`, ja kāda no vērtībām nav skaitlis
    */
  def average(first: String, second: String): String =
    (DataFrame.toNumber(first), DataFrame.toNumber(second)) match {
      case (Some(a), Some(b)) => Format.number((a + b) / 2)
      case _                  => ""
    }

  /** Pievieno kolonnu ar divu kolonnu vidējo vērtību.
    *
    * @param frame   datu rāmis
    * @param first   pirmās kolonnas nosaukums (pēc noklusējuma `DAX`)
    * @param second  otrās kolonnas nosaukums (pēc noklusējuma `FTSE`)
    * @param newName jaunās kolonnas nosaukums
    * @return jauns datu rāmis ar papildu kolonnu beigās vai `Left`, ja kāda
    *         kolonna nav atrasta vai jaunās kolonnas nosaukums jau ir aizņemts
    */
  def addAverageColumn(
      frame: DataFrame,
      first: String = "DAX",
      second: String = "FTSE",
      newName: String = averageColumn
  ): Either[String, DataFrame] =
    for {
      i      <- frame.requireColumn(first)
      j      <- frame.requireColumn(second)
      result <- frame.withColumn(newName, frame.rows.map(row => average(row(i), row(j))))
    } yield result

  private val added: Either[String, DataFrame] = addAverageColumn(Samples.frame)

  def tests: Vector[TestResult] = Vector(
    check(
      "Average of DAX and FTSE",
      "DAX: 0.02, 0.01, -0.04, NA, 0.03 / FTSE: 0.04, -0.03, 0.02, 0.01, 0.01",
      Right(Vector("0.03", "-0.01", "-0.01", "", "0.02")),
      added.flatMap(frame => Samples.column(frame, averageColumn))
    ),
    check(
      "New column is added at the end",
      "6 columns",
      Right(Samples.columns :+ averageColumn),
      added.map(_.columns)
    ),
    check("Value is missing (NA)", "NA and 0.01", "", average("NA", "0.01")),
    check("Empty cell", "\"\" and 0.2", "", average("", "0.2")),
    check("Letters instead of a number", "0.1 and abc", "", average("0.1", "abc")),
    check("Opposite numbers", "-0.5 and 0.5", "0", average("-0.5", "0.5")),
    check("Values with 9 decimals (as in the data set)", "0.002193419 and 0.003894376", "0.0030438975",
      average("0.002193419", "0.003894376")),
    check(
      "Column names in lower case",
      "dax, ftse",
      Right(Vector("0.03", "-0.01", "-0.01", "", "0.02")),
      addAverageColumn(Samples.frame, "dax", "ftse").flatMap(frame => Samples.column(frame, averageColumn))
    ),
    check(
      "Column does not exist",
      "DAXX",
      Left("column 'DAXX' not found (columns: date, ISE, DAX, FTSE, NIKKEI, EM)"),
      addAverageColumn(Samples.frame, "DAXX").map(_.columnCount)
    ),
    check(
      "New column name is already used",
      "new name ISE",
      Left("column 'ISE' already exists"),
      addAverageColumn(Samples.frame, "DAX", "FTSE", "ISE").map(_.columnCount)
    ),
    check("Empty data frame", "0 rows", Right(0), addAverageColumn(Samples.frame.head(0)).map(_.rowCount))
  )

  def run(data: Dataset): Unit =
    addAverageColumn(data.frame) match {
      case Left(error) => println(s"Error: $error")
      case Right(result) =>
        println(result.show(5))
        Samples.column(result, averageColumn).foreach { values =>
          val missing = values.count(_.isEmpty)
          println(s"Rows without an average (missing DAX or FTSE): $missing")
        }
    }
}
