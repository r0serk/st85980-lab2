package lab2

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.util.{Failure, Success, Try, Using}
import Testing.check

/** 10. uzdevums: saglabāt datu rāmi atpakaļ failu sistēmā `.csv` formātā.
  *
  * Mērķis: saglabāt apstrādāto datu rāmi, lai to varētu atvērt citā
  * programmā (piem. Excel). Tiek saglabāts datu rāmis pēc 5., 6. un
  * 8. uzdevuma: sakārtots pēc datuma, bez EM kolonnas un ar DAX un FTSE
  * vidējo vērtību. Pēc saglabāšanas fails tiek nolasīts vēlreiz, lai
  * pārbaudītu, ka dati nav zaudēti.
  */
object Task10 extends LabTask {

  val number: Int   = 10
  val title: String = "Save the data frame as CSV"

  /** Faila ceļš, ja lietotājs nospiež tikai Enter. */
  val defaultPath: Path = Paths.get("output", "istanbul_result.csv")

  private val forbidden: String = "<>\"|?*"

  /** Saglabājamais datu rāmis: 5. uzdevums -> 6. uzdevums -> 8. uzdevums.
    *
    * @param frame nolasītais datu rāmis
    * @return apstrādātais datu rāmis vai `Left`, ja kāda kolonna nav atrasta
    */
  def processed(frame: DataFrame): Either[String, DataFrame] =
    for {
      sorted  <- Task5.sortByDateDescending(frame)
      reduced <- Task6.removeColumn(sorted)
      result  <- Task8.addAverageColumn(reduced)
    } yield result

  /** Pārveido lietotāja ievadīto faila nosaukumu ceļā.
    *
    * Tukša ievade nozīmē [[defaultPath]]. Ja nosaukums nebeidzas ar `.csv`,
    * paplašinājums tiek pievienots. Pēdiņas sākumā un beigās tiek noņemtas.
    *
    * @param raw lietotāja ievade
    * @return `Right(ceļš)` vai `Left`, ja nosaukumā ir neatļauti simboli
    */
  def parseFileName(raw: String): Either[String, Path] = {
    val text = raw.trim.stripPrefix("\"").stripSuffix("\"").trim
    if (text.isEmpty) Right(defaultPath)
    else if (text.exists(c => forbidden.contains(c) || c.isControl))
      Left(s"'$text' contains characters that are not allowed in file names " + "(< > \" | ? *)")
    else if (text.endsWith("/") || text.endsWith("\\")) Left(s"'$text' is a folder name, a file name was expected")
    else {
      val withExtension = if (text.toLowerCase.endsWith(".csv")) text else text + ".csv"
      Try(Paths.get(withExtension)) match {
        case Success(path)  => Right(path)
        case Failure(error) => Left(s"'$text' is not a valid path (${error.getMessage})")
      }
    }
  }

  /** Saglabā datu rāmi CSV failā.
    *
    * @param frame datu rāmis
    * @param path  faila ceļš; trūkstošās mapes tiek izveidotas
    * @return `Right(ceļš)` vai `Left`, ja ceļš ir mape vai failu nevar saglabāt
    */
  def save(frame: DataFrame, path: Path): Either[String, Path] =
    if (Files.isDirectory(path)) Left(s"$path is a folder, not a file")
    else Csv.write(frame, path)

  /** Saglabā datu rāmi un nolasa to atpakaļ.
    *
    * @param frame datu rāmis
    * @param path  faila ceļš
    * @return `Right(true)`, ja nolasītais datu rāmis ir tieši tāds pats kā saglabātais
    */
  def roundTrip(frame: DataFrame, path: Path): Either[String, Boolean] =
    for {
      saved  <- save(frame, path)
      loaded <- Csv.read(saved)
    } yield loaded.frame == frame

  private val special = DataFrame(
    Vector("name", "note"),
    Vector(Vector("a,b", "say \"hi\""), Vector("", "x"))
  )

  def tests: Vector[TestResult] = Testing.withTempFolder { folder =>
    val file = folder.resolve("result.csv")
    Vector(
      check("Save and read back the sample data frame", "5 rows, 6 columns", Right(true), roundTrip(Samples.frame, file)),
      check(
        "First line of the file is the header",
        "result.csv",
        "date,ISE,DAX,FTSE,NIKKEI,EM",
        Files.readAllLines(file, StandardCharsets.UTF_8).get(0)
      ),
      check(
        "Values with a comma, quotes and an empty value",
        "\"a,b\", say \"hi\", \"\"",
        Right(true),
        roundTrip(special, folder.resolve("special.csv"))
      ),
      check(
        "Processed data frame (tasks 5, 6, 8)",
        "sorted, without EM, with DAX_FTSE_AVG",
        Right(Vector("date", "ISE", "DAX", "FTSE", "NIKKEI", "DAX_FTSE_AVG")),
        processed(Samples.frame).flatMap(frame => save(frame, folder.resolve("p.csv"))).flatMap(Csv.read).map(_.frame.columns)
      ),
      check(
        "Missing folders are created",
        "new/sub/folder/out.csv",
        true,
        save(Samples.frame, folder.resolve("new").resolve("sub").resolve("folder").resolve("out.csv")).isRight &&
          Files.exists(folder.resolve("new").resolve("sub").resolve("folder").resolve("out.csv"))
      ),
      check("Folder instead of a file", "existing folder", Left(s"$folder is a folder, not a file"), save(Samples.frame, folder)),
      check("Empty file name", "\"\"", Right(defaultPath), parseFileName("")),
      check("Name without .csv", "result", Right(Paths.get("result.csv")), parseFileName("result")),
      check("Name with .CSV", "data.CSV", Right(Paths.get("data.CSV")), parseFileName("data.CSV")),
      check("Name in quotes", "\"out.csv\"", Right(Paths.get("out.csv")), parseFileName("\"out.csv\"")),
      check(
        "Forbidden characters",
        "bad?name",
        Left("'bad?name' contains characters that are not allowed in file names (< > \" | ? *)"),
        parseFileName("bad?name")
      ),
      check("Folder name", "output/", Left("'output/' is a folder name, a file name was expected"), parseFileName("output/"))
    )
  }

  // Jautā faila nosaukumu, līdz tas ir korekts; ja fails jau eksistē, jautā, vai to pārrakstīt.
  @annotation.tailrec
  private def askPath(): Option[Path] =
    Input.promptUntilValid(s"File name (press Enter for $defaultPath): ", parseFileName) match {
      case None => None
      case Some(path) =>
        if (Files.exists(path) && !Files.isDirectory(path) && !Input.askYesNo(s"$path already exists. Overwrite it? (y/n): "))
          askPath()
        else Some(path)
    }

  def run(data: Dataset): Unit =
    processed(data.frame) match {
      case Left(error) => println(s"Error: $error")
      case Right(result) =>
        println(s"Data frame to save: ${result.rowCount} rows, ${result.columnCount} columns")
        println(s"Columns: ${result.columns.mkString(", ")}")
        println()
        askPath() match {
          case None => println("Nothing was saved.")
          case Some(path) =>
            save(result, path).flatMap(saved => Csv.read(saved).map(loaded => (saved, loaded.frame))) match {
              case Left(error) => println(s"Error: $error")
              case Right((saved, loaded)) =>
                println(s"Saved: ${saved.toAbsolutePath}")
                println(s"Read back: ${loaded.rowCount} rows, ${loaded.columnCount} columns")
                println(s"Same as the saved data frame: ${if (loaded == result) "yes" else "NO"}")
                println()
                println("First lines of the file:")
                Using(scala.io.Source.fromFile(saved.toFile, "UTF-8"))(source => source.getLines().take(4).toVector)
                  .getOrElse(Vector.empty)
                  .foreach(line => println(s"  $line"))
            }
        }
    }
}
