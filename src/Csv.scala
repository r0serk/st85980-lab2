package lab2

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.util.{Failure, Success, Try}

/** CSV failu nolasīšana un saglabāšana.
  *
  * Apstrādātie robežgadījumi:
  *  - fails neeksistē, ir mape vai ir tukšs;
  *  - atdalītājs `,`, `;` vai tabulācija (tiek noteikts automātiski);
  *  - vērtības pēdiņās, arī ar komatu vai pēdiņām iekšā (`"a,b"`, `""`);
  *  - UTF-8 BOM faila sākumā un Windows rindu beigas (`\r\n`);
  *  - tukšas rindas (tiek izlaistas);
  *  - rindas ar neaizvērtām pēdiņām (tiek izlaistas un uzskaitītas).
  *
  * Galvenes apstrāde un datu rāmja izveide ir kopīga ar Excel failiem,
  * skatīt [[DataFile.fromRows]].
  */
object Csv {

  /** Atbalstītie atdalītāji. */
  val separators: Vector[Char] = Vector(',', ';', '\t')

  /** Nosaka atdalītāju pēc galvenes rindas: to, kas sastopams visbiežāk.
    *
    * @param headerLine faila pirmā rinda
    * @return atdalītājs; ja neviena nav, tad `,`
    */
  def detectSeparator(headerLine: String): Char =
    separators.maxBy(separator => headerLine.count(_ == separator))

  /** Atdalītāja nosaukums izdrukai (tabulācija tiek parādīta kā `\t`). */
  def separatorName(separator: Char): String =
    if (separator == '\t') "\\t" else separator.toString

  /** Sadala vienu CSV rindu vērtībās, ņemot vērā pēdiņas.
    *
    * @param line      CSV rinda
    * @param separator atdalītājs
    * @return vērtību vektors vai `Left`, ja pēdiņas nav aizvērtas
    *
    * Sarežģītība: O(n), kur n ir rindas garums.
    */
  def splitLine(line: String, separator: Char): Either[String, Vector[String]] = {
    @annotation.tailrec
    def go(i: Int, inQuotes: Boolean, cell: String, cells: Vector[String]): Either[String, Vector[String]] =
      if (i >= line.length) {
        if (inQuotes) Left("unclosed quotation mark") else Right(cells :+ cell)
      } else {
        val c = line.charAt(i)
        if (inQuotes) {
          if (c == '"' && i + 1 < line.length && line.charAt(i + 1) == '"') go(i + 2, true, cell + '"', cells)
          else if (c == '"') go(i + 1, false, cell, cells)
          else go(i + 1, true, cell + c, cells)
        } else {
          if (c == '"') go(i + 1, true, cell, cells)
          else if (c == separator) go(i + 1, false, "", cells :+ cell)
          else go(i + 1, false, cell + c, cells)
        }
      }
    go(0, false, "", Vector.empty)
  }

  /** Pārveido CSV tekstu datu rāmī.
    *
    * @param text visa CSV faila saturs
    * @return [[LoadResult]] vai `Left`, ja teksts ir tukšs vai galveni nevar nolasīt
    */
  def parse(text: String): Either[String, LoadResult] = {
    val numberedLines = text
      .stripPrefix("\uFEFF")
      .split("\r?\n", -1)
      .toVector
      .zipWithIndex
      .map { case (line, index) => (index + 1, line) }
      .filter { case (_, line) => line.trim.nonEmpty }

    numberedLines.headOption match {
      case None => Left("the file is empty")
      case Some((_, headerLine)) =>
        val separator = detectSeparator(headerLine)
        val rows      = numberedLines.map { case (lineNumber, line) => (lineNumber, splitLine(line, separator)) }
        DataFile.fromRows(rows, s"CSV (separator '${separatorName(separator)}')")
    }
  }

  /** Nolasa CSV failu.
    *
    * @param path faila ceļš
    * @return [[LoadResult]] vai `Left`, ja fails neeksistē, ir mape, to nevar
    *         nolasīt vai tas ir tukšs
    */
  def read(path: Path): Either[String, LoadResult] =
    if (!Files.exists(path)) Left(s"file not found: $path")
    else if (Files.isDirectory(path)) Left(s"$path is a folder, not a file")
    else
      Try(new String(Files.readAllBytes(path), StandardCharsets.UTF_8)) match {
        case Success(text)  => parse(text)
        case Failure(error) => Left(s"cannot read the file: ${describe(error)}")
      }

  /** Pārveido vienu vērtību CSV formātā: pēdiņās, ja tā satur komatu,
    * pēdiņas, rindas beigas vai atstarpes sākumā/beigās.
    *
    * @param cell vērtība
    * @return vērtība CSV formātā, piem. `a,b` kļūst par `"a,b"`
    */
  def quote(cell: String): String =
    if (cell.exists(c => c == ',' || c == '"' || c == '\n' || c == '\r') || cell != cell.trim)
      "\"" + cell.replace("\"", "\"\"") + "\""
    else cell

  /** Pārveido vērtību vektoru vienā CSV rindā (atdalītājs - komats). */
  def toLine(cells: Vector[String]): String = cells.map(quote).mkString(",")

  /** Saglabā datu rāmi CSV failā (UTF-8, atdalītājs - komats).
    * Ja mape neeksistē, tā tiek izveidota.
    *
    * @param frame saglabājamais datu rāmis
    * @param path  faila ceļš
    * @return `Right(ceļš)` vai `Left`, ja failu nevar saglabāt
    */
  def write(frame: DataFrame, path: Path): Either[String, Path] = {
    val content = (frame.columns +: frame.rows).map(toLine).mkString("", "\n", "\n")
    val saved = Try {
      Option(path.toAbsolutePath.getParent).foreach(parent => Files.createDirectories(parent))
      Files.write(path, content.getBytes(StandardCharsets.UTF_8))
      path
    }
    saved match {
      case Success(savedPath) => Right(savedPath)
      case Failure(error)     => Left(s"could not save the file: ${describe(error)}")
    }
  }

  private def describe(error: Throwable): String =
    s"${error.getClass.getSimpleName}: ${Option(error.getMessage).getOrElse("")}"
}
