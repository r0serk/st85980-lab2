package lab2

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Viena testa rezultāts.
  *
  * @param name     testa nosaukums (kas tiek pārbaudīts)
  * @param input    ievaddati teksta formā
  * @param expected gaidītais rezultāts teksta formā
  * @param actual   faktiskais rezultāts teksta formā
  * @param passed   vai faktiskais rezultāts sakrīt ar gaidīto
  */
final case class TestResult(name: String, input: String, expected: String, actual: String, passed: Boolean)

/** Testa gadījumu izveide un rezultātu izdruka.
  *
  * Katrs uzdevums apraksta savus testus, arī robežgadījumus, un tie tiek
  * izdrukāti vienotā formātā:
  * {{{
  * Test: Sort by date (descending)
  * Input: 5 rows with dates in different order
  * Expected: ["12-Feb-10", "10-Jan-09", ...]
  * Actual: ["12-Feb-10", "10-Jan-09", ...]
  * Result: PASS
  * }}}
  * Ārējas testēšanas bibliotēkas netiek izmantotas, lai programmai
  * nebūtu vajadzīgas papildu atkarības.
  */
object Testing {

  /** Izveido testa rezultātu, salīdzinot gaidīto un faktisko vērtību.
    *
    * @param name     testa nosaukums
    * @param input    ievaddatu apraksts
    * @param expected gaidītā vērtība
    * @param actual   funkcijas faktiski atgrieztā vērtība
    * @return aizpildīts [[TestResult]]
    */
  def check[A](name: String, input: String, expected: A, actual: A): TestResult =
    TestResult(name, input, render(expected), render(actual), expected == actual)

  /** Attēlo vērtību tekstā testa izdrukai.
    *
    * Teksts tiek likts pēdiņās, skaitļi formatēti ar [[Format.number]],
    * saraksti - kvadrātiekavās.
    *
    * @param value attēlojamā vērtība
    * @return vērtības teksts
    */
  def render(value: Any): String = value match {
    case text: String   => "\"" + text + "\""
    case number: Double => Format.number(number)
    case Left(error)    => "Left(" + render(error) + ")"
    case Right(result)  => "Right(" + render(result) + ")"
    case Some(inner)    => "Some(" + render(inner) + ")"
    case None           => "None"
    case items: Seq[?]  => items.map(render).mkString("[", ", ", "]")
    case other          => String.valueOf(other)
  }

  /** Izdrukā testu grupu un kopsavilkumu.
    *
    * @param title testu grupas nosaukums
    * @param tests testu rezultāti
    * @return `true`, ja visi testi ir izturēti
    */
  def runTests(title: String, tests: Vector[TestResult]): Boolean = {
    println(s"=== $title ===")
    println()
    tests.foreach { test =>
      println(s"Test: ${test.name}")
      println(s"Input: ${test.input}")
      println(s"Expected: ${test.expected}")
      println(s"Actual: ${test.actual}")
      println(s"Result: ${if (test.passed) "PASS" else "FAIL"}")
      println()
    }
    val passed = tests.count(_.passed)
    println(s"Tests passed: $passed/${tests.size}")
    println()
    passed == tests.size
  }

  /** Izpilda testus pagaidu mapē, kas pēc tam tiek izdzēsta (arī kļūdas gadījumā).
    *
    * @param body testi, kas saņem pagaidu mapes ceļu
    * @return testu rezultāts
    */
  def withTempFolder[A](body: Path => A): A = {
    val folder = Files.createTempDirectory("lab2-test")
    try body(folder)
    finally deleteRecursively(folder)
  }

  private def deleteRecursively(path: Path): Unit = {
    if (Files.isDirectory(path)) {
      val children = Using(Files.list(path))(stream => stream.iterator().asScala.toVector).getOrElse(Vector.empty)
      children.foreach(deleteRecursively)
    }
    Files.deleteIfExists(path)
    ()
  }
}
