package lab2

import Testing.check

/** 3. uzdevums: izvadīt tikai datu rāmja galveni (header).
  *
  * Mērķis: parādīt kolonnu nosaukumus bez datiem. Kolonnas tiek arī
  * numurētas, jo 9. uzdevumā kolonna tiek izvēlēta pēc numura.
  */
object Task3 extends LabTask {

  val number: Int   = 3
  val title: String = "Show the header"

  /** Atgriež datu rāmja galveni.
    *
    * @param frame datu rāmis
    * @return kolonnu nosaukumi tādā secībā kā failā
    */
  def header(frame: DataFrame): Vector[String] = frame.columns

  /** Atgriež numurētu kolonnu sarakstu (numurs sākas ar 1).
    *
    * @param frame datu rāmis
    * @return piem. `Vector("1. date", "2. ISE", ...)`
    */
  def numbered(frame: DataFrame): Vector[String] =
    header(frame).zipWithIndex.map { case (name, index) => s"${index + 1}. $name" }

  def tests: Vector[TestResult] = Vector(
    check("Header of the sample data frame", "6 columns, 5 rows", Samples.columns, header(Samples.frame)),
    check("Header does not depend on rows", "same columns, 0 rows", Samples.columns, header(Samples.frame.head(0))),
    check(
      "Numbered header starts from 1",
      "6 columns",
      Vector("1. date", "2. ISE", "3. DAX", "4. FTSE", "5. NIKKEI", "6. EM"),
      numbered(Samples.frame)
    ),
    check("Data frame without columns", "0 columns", Vector.empty[String], header(DataFrame(Vector.empty, Vector.empty)))
  )

  def run(data: Dataset): Unit = {
    val columns = header(data.frame)
    println(s"Header (${columns.size} columns):")
    println(columns.mkString(", "))
    println()
    println("Column numbers (used in task 9):")
    numbered(data.frame).foreach(line => println(s"  $line"))
  }
}
