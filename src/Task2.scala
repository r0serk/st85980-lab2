package lab2

import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try}
import Testing.check

/** 2. uzdevums: nolasīt datu kopu ar Scala.
  *
  * Mērķis: nolasīt Stambulas biržas datu kopu datu rāmī (`val df = ...`),
  * ar kuru strādā pārējie uzdevumi. Kaggle datu kopa ir Excel fails
  * (`ISTANBUL STOCK EXCHANGE Data Set.xlsx`), tāpēc tiek atbalstīti gan `.xlsx` ([[Xlsx]]),
  * gan `.csv` ([[Csv]]) faili. Šeit ir uzdevuma funkcijas, testi un
  * rezultāta izdruka.
  */
object Task2 extends LabTask {

  val number: Int   = 2
  val title: String = "Read the data set"

  /** Nolasa datu failu (`.xlsx` vai `.csv`) datu rāmī.
    *
    * @param path faila ceļš
    * @return `Right(datu rāmis)` vai `Left` ar kļūdas aprakstu
    */
  def readDataFrame(path: Path): Either[String, DataFrame] =
    DataFile.read(path).map(_.frame)

  /** Pārveido lietotāja ievadīto tekstu faila ceļā.
    *
    * Windows "Copy as path" ceļu ieliek pēdiņās (`"C:\data\file.csv"`),
    * tāpēc pēdiņas sākumā un beigās tiek noņemtas.
    *
    * @param raw lietotāja ievadītais ceļš
    * @return `Right(ceļš)` vai `Left`, ja ievade ir tukša vai ceļš nav korekts
    */
  def parsePath(raw: String): Either[String, Path] = {
    val text = raw.trim.stripPrefix("\"").stripSuffix("\"").trim
    if (text.isEmpty) Left("empty input - a file path was expected")
    else
      Try(Paths.get(text)) match {
        case Success(path)  => Right(path)
        case Failure(error) => Left(s"'$text' is not a valid path (${error.getMessage})")
      }
  }

  /** Īss nolasīšanas rezultāta apraksts (izmanto testos un izdrukā).
    *
    * @param result [[DataFile.read]] vai [[Csv.parse]] rezultāts
    * @return piem. `columns: [date, ISE]; rows: 2; skipped: 0`
    *         vai `error: the file is empty`
    */
  def describe(result: Either[String, LoadResult]): String = result match {
    case Left(error) => s"error: $error"
    case Right(loaded) =>
      val frame = loaded.frame
      s"columns: ${frame.columns.mkString("[", ", ", "]")}; rows: ${frame.rowCount}; skipped: ${loaded.skipped.size}"
  }

  // Excel faila testi: pagaidu mapē tiek izveidots neliels .xlsx fails (Samples.xlsxParts).
  private def excelTests(): Vector[TestResult] = Testing.withTempFolder { folder =>
    val xlsx = folder.resolve("sample.xlsx")
    Samples.writeXlsx(xlsx)
    val notZip = folder.resolve("broken.xlsx")
    Files.write(notZip, "this is not a zip file".getBytes("UTF-8"))
    val old = folder.resolve("old.xls")
    Files.write(old, Array[Byte](1, 2, 3))
    Vector(
      check(
        "Excel file with a two-level header",
        "sample.xlsx: TL BASED / USD BASED over two ISE columns",
        "columns: [date, ISE (TL BASED), ISE (USD BASED), EM]; rows: 2; skipped: 0",
        describe(DataFile.read(xlsx))
      ),
      check(
        "Excel values: dates, numbers, missing cells",
        "39818 (date format), 3.8376187000000002E-2, empty C4 and D3",
        Right(Vector(
          Vector("5-Jan-09", "0.035753708", "0.038376187", ""),
          Vector("6-Jan-09", "-0.0254", "", "0.0123")
        )),
        DataFile.read(xlsx).map(_.frame.rows)
      ),
      check("Excel file format is reported", "sample.xlsx", Right("Excel (.xlsx), sheet 'Sheet1'"), DataFile.read(xlsx).map(_.format)),
      check("Excel date number", "39818", "5-Jan-09", Xlsx.excelDate(39818, false)),
      check("Excel date number (1904 date system)", "0", "1-Jan-1904", Xlsx.excelDate(0, true)),
      check("Date format code", "[$-409]d\\-mmm\\-yy;@", true, Xlsx.isDateCode("[$-409]d\\-mmm\\-yy;@")),
      check("Number format code is not a date", "0.00", false, Xlsx.isDateCode("0.00")),
      check("Column letters", "AB12", Some(27), Xlsx.columnIndex("AB12")),
      check(
        "File with .xlsx extension is not an Excel file",
        "broken.xlsx (plain text)",
        true,
        describe(DataFile.read(notZip)).startsWith("error: cannot read the Excel file")
      ),
      check(
        "Old Excel format",
        "old.xls",
        "error: the old Excel format (.xls) is not supported - save the file as .xlsx or .csv",
        describe(DataFile.read(old))
      )
    )
  }

  def tests: Vector[TestResult] = Vector(
    check(
      "Normal CSV (comma separator)",
      "date,ISE,DAX + 2 data lines",
      "columns: [date, ISE, DAX]; rows: 2; skipped: 0",
      describe(Csv.parse("date,ISE,DAX\n5-Jan-09,0.01,0.02\n6-Jan-09,-0.01,0.03\n"))
    ),
    check(
      "Semicolon separator is detected",
      "date;ISE + 1 data line",
      Right("CSV (separator ';')"),
      Csv.parse("date;ISE\n5-Jan-09;0.01").map(_.format)
    ),
    check(
      "Value in quotes with a comma inside",
      "name,value / \"a,b\",1",
      Right("a,b"),
      Csv.parse("name,value\n\"a,b\",1").map(_.frame.rows.head.head)
    ),
    check(
      "BOM, Windows line endings and an empty line",
      "BOM + date,ISE + empty line + 5-Jan-09,0.01 (CRLF line endings)",
      "columns: [date, ISE]; rows: 1; skipped: 0",
      describe(Csv.parse("\uFEFFdate,ISE\r\n\r\n5-Jan-09,0.01\r\n"))
    ),
    check(
      "Two-level header (as in the UCI version of the file)",
      ",TL BASED,USD BASED, / date,ISE,ISE,EM / 5-Jan-09,0.03,0.04,0.02",
      "columns: [date, ISE (TL BASED), ISE (USD BASED), EM]; rows: 1; skipped: 0",
      describe(Csv.parse(",TL BASED,USD BASED,\ndate,ISE,ISE,EM\n5-Jan-09,0.03,0.04,0.02"))
    ),
    check(
      "Repeated column names get a number",
      "a,a,b / 1,2,3",
      Right(Vector("a", "a.1", "b")),
      Csv.parse("a,a,b\n1,2,3").map(_.frame.columns)
    ),
    check(
      "Empty column name gets a name",
      "date,,x / 1-Jan-09,1,2",
      Right(Vector("date", "column_2", "x")),
      Csv.parse("date,,x\n1-Jan-09,1,2").map(_.frame.columns)
    ),
    check(
      "Lines with a wrong number of values are skipped",
      "date,ISE / 5-Jan-09,0.01 / 6-Jan-09 / 7-Jan-09,0.02,9",
      Right(Vector("line 3: 1 values, expected 2", "line 4: 3 values, expected 2")),
      Csv.parse("date,ISE\n5-Jan-09,0.01\n6-Jan-09\n7-Jan-09,0.02,9").map(_.skipped)
    ),
    check(
      "Unclosed quotation mark",
      "a,b / \"x,1",
      Right(Vector("line 2: unclosed quotation mark")),
      Csv.parse("a,b\n\"x,1").map(_.skipped)
    ),
    check("Empty file", "\"\"", "error: the file is empty", describe(Csv.parse(""))),
    check("File with only spaces and empty lines", "\"  \\n \\n\"", "error: the file is empty", describe(Csv.parse("  \n \n"))),
    check(
      "File does not exist",
      "no_such_file_12345.csv",
      "error: file not found: no_such_file_12345.csv",
      describe(DataFile.read(Paths.get("no_such_file_12345.csv")))
    ),
    check("Folder instead of a file", ".", "error: . is a folder, not a file", describe(DataFile.read(Paths.get(".")))),
    check(
      "Path in quotes (Windows \"Copy as path\")",
      "\"data/file.csv\"",
      Right(Paths.get("data/file.csv")),
      parsePath("\"data/file.csv\"")
    ),
    check("Empty path", "\"\"", Left("empty input - a file path was expected"), parsePath("   "))
  ) ++ excelTests()

  def run(data: Dataset): Unit = {
    println("val df = Task2.readDataFrame(Paths.get(\"" + data.path + "\"))")
    println()
    DataFile.read(data.path) match {
      case Left(error) => println(s"Error: $error")
      case Right(loaded) =>
        val df: DataFrame = loaded.frame
        val dateIndex     = df.columnIndex("date")
        val validDates    = dateIndex.map(i => df.rows.count(row => Dates.parse(row(i)).isDefined))
        println(s"File:          ${data.path.toAbsolutePath}")
        println(s"Format:        ${loaded.format}")
        println(s"Rows:          ${df.rowCount}")
        println(s"Columns:       ${df.columnCount}")
        println(s"Skipped lines: ${loaded.skipped.size}")
        loaded.skipped.take(5).foreach(line => println(s"  - $line"))
        validDates.foreach(count => println(s"Valid dates:   $count/${df.rowCount}"))
        println()
        println("The data frame is loaded into memory (df).")
    }
  }
}
