package lab2

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Laboratorijas darbs Nr. 2 (Scala), 1. variants: Stambulas biržas datu kopa.
  *
  * Programmas palaišana no projekta mapes:
  * {{{
  * scala-cli run .                          // izvēlne; fails tiek meklēts mapē data/
  * scala-cli run . -- data/fails.csv        // izvēlne ar norādīto failu
  * scala-cli run . -- --test                // tikai testi (bez izvēlnes)
  * }}}
  *
  * Datu kopas fails tiek meklēts šādā secībā: komandrindas arguments,
  * `.xlsx` vai `.csv` fails mapē `data/`, lietotāja ievadīts ceļš.
  */
object Main {

  /** Visi uzdevumi izvēlnes secībā (1. uzdevums ir datu lejupielāde no Kaggle). */
  val tasks: Vector[LabTask] = Vector(Task2, Task3, Task4, Task5, Task6, Task7, Task8, Task9, Task10)

  /** Mape, kurā tiek meklēts datu kopas fails. */
  val dataFolder: Path = Paths.get("data")

  /** Kolonnas, kuras izmanto uzdevumi (lai brīdinātu, ja failā to nav). */
  val requiredColumns: Vector[String] = Vector("date", "EM", "NIKKEI", "DAX", "FTSE")

  /** Programmas sākums.
    *
    * @param args `--test` - izpildīt tikai testus; cits arguments - datu kopas ceļš
    */
  def main(args: Array[String]): Unit = {
    val options = args.toVector
    if (options.contains("--test")) {
      val passed = runAllTests(options.filterNot(_.startsWith("--")))
      sys.exit(if (passed) 0 else 1)
    } else {
      printTitle()
      loadDataset(options.filterNot(_.startsWith("--"))) match {
        case None       => println("No data set was loaded. Goodbye!")
        case Some(data) => menuLoop(data)
      }
    }
  }

  /** Pārbauda izvēlnes ievadi.
    *
    * @param raw lietotāja ievade
    * @return uzdevuma numurs (vai 0 - iziet) vai `Left` ar kļūdas aprakstu
    */
  def parseChoice(raw: String): Either[String, Int] =
    Input.parseInt(raw).flatMap { choice =>
      if (choice == 0 || tasks.exists(_.number == choice)) Right(choice)
      else if (choice == 1) Left("task 1 (downloading the data set from Kaggle) is done outside the program")
      else Left(s"there is no task $choice (choose 0 or 2-10)")
    }

  /** Atrod datu kopas failu mapē `data/`.
    *
    * Excel pagaidu faili (`~$...`, kamēr fails ir atvērts Excel) tiek ignorēti.
    *
    * @return `data_akbilgic.xlsx` vai `.csv` (UCI faila nosaukums), ja tāds
    *         ir, citādi pirmais `.xlsx` vai `.csv` fails pēc nosaukuma;
    *         `None`, ja mapes vai faila nav
    */
  def findInDataFolder(): Option[Path] =
    if (!Files.isDirectory(dataFolder)) None
    else {
      val files = Using(Files.list(dataFolder))(stream => stream.iterator().asScala.toVector)
        .getOrElse(Vector.empty)
        .filter { path =>
          val name = path.getFileName.toString.toLowerCase
          Files.isRegularFile(path) && !name.startsWith("~$") && (name.endsWith(".xlsx") || name.endsWith(".csv"))
        }
        .sortBy(_.getFileName.toString)
      files.find(_.getFileName.toString.toLowerCase.startsWith("data_akbilgic.")).orElse(files.headOption)
    }

  private def printTitle(): Unit = {
    println("==============================================================")
    println(" Laboratory work 2 - Scala, variant 1 (Istanbul Stock Exchange)")
    println("==============================================================")
    println()
  }

  // Nolasa datu kopu: arguments -> mape data/ -> lietotāja ievade.
  private def loadDataset(args: Vector[String]): Option[Dataset] = {
    val candidate: Option[Path] =
      args.headOption.flatMap(arg => Task2.parsePath(arg).toOption).orElse(findInDataFolder())
    val loaded = candidate.flatMap { path =>
      println(s"Loading $path ...")
      DataFile.read(path) match {
        case Right(result) => Some(Dataset(path, result))
        case Left(error) =>
          println(s"  Error: $error")
          None
      }
    }
    val dataset = loaded.orElse {
      println(s"Put the data file from Kaggle (.xlsx or .csv) into the '$dataFolder' folder or enter its path.")
      Input.promptUntilValid(
        "Path to the data file: ",
        raw => Task2.parsePath(raw).flatMap(path => DataFile.read(path).map(result => Dataset(path, result)))
      )
    }
    dataset.foreach(warnAboutColumns)
    dataset
  }

  private def warnAboutColumns(data: Dataset): Unit = {
    val missing = requiredColumns.filter(name => data.frame.columnIndex(name).isEmpty)
    println(s"Loaded: ${data.frame.rowCount} rows, ${data.frame.columnCount} columns.")
    if (missing.nonEmpty)
      println(s"Warning: columns ${missing.mkString(", ")} were not found - tasks that need them will show an error.")
    println()
  }

  private def printMenu(data: Dataset): Unit = {
    println("--------------------------------------------------------------")
    println(s" Data set: ${data.path.getFileName} (${data.frame.rowCount} rows, ${data.frame.columnCount} columns)")
    println("--------------------------------------------------------------")
    tasks.foreach(task => println(f" ${task.number}%2d. ${task.title}"))
    println("  0. Exit")
    println()
  }

  private def runTask(task: LabTask, data: Dataset): Unit = {
    println()
    Testing.runTests(s"Task ${task.number}: ${task.title} - tests", task.tests)
    println(s"=== Task ${task.number}: ${task.title} - result with the data set ===")
    println()
    task.run(data)
  }

  @annotation.tailrec
  private def menuLoop(data: Dataset): Unit = {
    printMenu(data)
    Input.promptUntilValid("Choose a task (0, 2-10): ", parseChoice) match {
      case None | Some(0) => println("Goodbye!")
      case Some(choice) =>
        tasks.find(_.number == choice).foreach(task => runTask(task, data))
        Input.waitForEnter()
        menuLoop(data)
    }
  }

  private val menuTests: Vector[TestResult] = Vector(
    Testing.check("Menu: valid task", "5", Right(5), parseChoice("5")),
    Testing.check("Menu: exit", "0", Right(0), parseChoice("0")),
    Testing.check(
      "Menu: task 1",
      "1",
      Left("task 1 (downloading the data set from Kaggle) is done outside the program"),
      parseChoice("1")
    ),
    Testing.check("Menu: number out of range", "11", Left("there is no task 11 (choose 0 or 2-10)"), parseChoice("11")),
    Testing.check("Menu: letters", "abc", Left("'abc' is not a number (letters are not allowed)"), parseChoice("abc")),
    Testing.check("Menu: empty input", "\"\"", Left("empty input - a number was expected"), parseChoice(""))
  )

  // Režīms --test: izpilda visus testus un, ja datu kopa ir atrasta, pārbauda tās nolasīšanu.
  private def runAllTests(args: Vector[String]): Boolean = {
    val groups: Vector[(String, Vector[TestResult])] =
      Vector(("Main menu - tests", menuTests)) ++ tasks.map(task => (s"Task ${task.number}: ${task.title} - tests", task.tests))
    val results = groups.map { case (title, tests) => (Testing.runTests(title, tests), tests) }
    val total   = results.map(_._2.size).sum
    val passed  = results.map { case (_, tests) => tests.count(_.passed) }.sum

    val datasetOk = args.headOption.flatMap(arg => Task2.parsePath(arg).toOption).orElse(findInDataFolder()) match {
      case None =>
        println("Data set check: no .xlsx or .csv file in the data folder (skipped)")
        true
      case Some(path) =>
        DataFile.read(path) match {
          case Right(result) =>
            val missing = requiredColumns.filter(name => result.frame.columnIndex(name).isEmpty)
            println(s"Data set check: $path - ${result.frame.rowCount} rows, ${result.frame.columnCount} columns, " +
              s"${result.skipped.size} skipped lines")
            if (missing.nonEmpty) println(s"Data set check: missing columns ${missing.mkString(", ")}")
            missing.isEmpty && result.frame.rowCount > 0
          case Left(error) =>
            println(s"Data set check: FAIL - $error")
            false
        }
    }

    println()
    println(s"TOTAL: $passed/$total tests passed")
    val ok = passed == total && datasetOk
    println(if (ok) "ALL CHECKS PASSED" else "SOME CHECKS FAILED")
    ok
  }
}
