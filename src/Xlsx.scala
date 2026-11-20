package lab2

import java.math.MathContext
import java.nio.file.Path
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.Locale
import java.util.zip.ZipFile
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import org.w3c.dom.{Document, Element}
import scala.util.{Failure, Success, Try, Using}

/** Excel darbgrāmatas (`.xlsx`) nolasīšana bez ārējām bibliotēkām.
  *
  * `.xlsx` fails ir ZIP arhīvs ar XML failiem. Tiek izmantoti:
  *  - `xl/workbook.xml` - lapu saraksts (tiek nolasīta pirmā lapa);
  *  - `xl/_rels/workbook.xml.rels` - kurā failā atrodas lapa;
  *  - `xl/sharedStrings.xml` - teksta vērtības (šūnā ir tikai to numurs);
  *  - `xl/styles.xml` - šūnu formāti (lai atpazītu datumus);
  *  - `xl/worksheets/sheetN.xml` - lapas rindas un šūnas.
  *
  * Excel datumu glabā kā skaitli - dienu skaitu kopš 1899-12-30 (piem.
  * `39818` ir 2009. gada 5. janvāris). Ja šūnai ir datuma formāts, skaitlis
  * tiek pārveidots tekstā `5-Jan-09`. Pārējie skaitļi tiek pārveidoti
  * tekstā bez zinātniskā pieraksta (`3.5753708E-2` -> `0.035753708`).
  * Tukšas šūnas, kuras failā nav ierakstītas, kļūst par tukšu tekstu.
  */
object Xlsx {

  private final case class SheetRef(name: String, path: String)

  /** Nolasa `.xlsx` faila pirmo lapu.
    *
    * @param path faila ceļš
    * @return [[LoadResult]] vai `Left`, ja fails nav korekts `.xlsx` fails
    */
  def read(path: Path): Either[String, LoadResult] =
    Try(Using.resource(new ZipFile(path.toFile))(zip => readWorkbook(zip))) match {
      case Success(result) => result
      case Failure(error) =>
        Left(s"cannot read the Excel file: ${error.getClass.getSimpleName}: ${Option(error.getMessage).getOrElse("")}")
    }

  /** Pārveido Excel datuma skaitli tekstā.
    *
    * @param serial   dienu skaits (daļa aiz komata ir laiks un tiek atmesta)
    * @param date1904 `true`, ja darbgrāmata izmanto 1904. gada datumu sistēmu
    * @return datums formā `5-Jan-09` (gadiem 2000-2099) vai `5-Jan-1999`
    */
  def excelDate(serial: Double, date1904: Boolean): String = {
    val base    = if (date1904) LocalDate.of(1904, 1, 1) else LocalDate.of(1899, 12, 30)
    val date    = base.plusDays(math.floor(serial).toLong)
    val pattern = if (date.getYear >= 2000 && date.getYear <= 2099) "d-MMM-yy" else "d-MMM-yyyy"
    date.format(DateTimeFormatter.ofPattern(pattern, Locale.ENGLISH))
  }

  /** Pārbauda, vai Excel skaitļa formāts ir datuma formāts.
    *
    * @param code formāta kods, piem. `[$-409]d\-mmm\-yy;@` vai `0.00`
    * @return `true`, ja formātā ir diena, mēnesis vai gads
    */
  def isDateCode(code: String): Boolean = {
    val cleaned = code
      .replaceAll("\"[^\"]*\"", "")
      .replaceAll("\\[[^\\]]*\\]", "")
      .replaceAll("\\\\.", "")
      .toLowerCase
    cleaned.exists(c => c == 'd' || c == 'y') || (cleaned.contains('m') && !cleaned.exists(c => c == 'h' || c == 's'))
  }

  /** Pārveido šūnas adreses burtus kolonnas indeksā.
    *
    * @param reference šūnas adrese, piem. `A1` vai `AB12`
    * @return indekss no 0 (`A` -> 0, `AB` -> 27) vai `None`, ja burtu nav
    */
  def columnIndex(reference: String): Option[Int] = {
    val letters = reference.takeWhile(_.isLetter).toUpperCase
    if (letters.isEmpty) None
    else Some(letters.foldLeft(0)((acc, c) => acc * 26 + (c - 'A' + 1)) - 1)
  }

  private def readWorkbook(zip: ZipFile): Either[String, LoadResult] =
    xml(zip, "xl/workbook.xml") match {
      case None => Left("xl/workbook.xml not found - this is not an .xlsx file")
      case Some(workbook) =>
        firstSheet(zip, workbook).flatMap { sheet =>
          xml(zip, sheet.path) match {
            case None => Left(s"${sheet.path} not found in the file")
            case Some(sheetXml) =>
              val strings    = xml(zip, "xl/sharedStrings.xml").map(sharedStrings).getOrElse(Vector.empty)
              val dateStyles = xml(zip, "xl/styles.xml").map(dateStyleIndexes).getOrElse(Set.empty[Int])
              val date1904 = elements(workbook.getDocumentElement, "workbookPr").headOption
                .exists(e => e.getAttribute("date1904") == "1" || e.getAttribute("date1904") == "true")
              DataFile.fromRows(sheetRows(sheetXml, strings, dateStyles, date1904), s"Excel (.xlsx), sheet '${sheet.name}'")
          }
        }
    }

  // Pirmā lapa un tās XML faila ceļš arhīvā.
  private def firstSheet(zip: ZipFile, workbook: Document): Either[String, SheetRef] =
    elements(workbook.getDocumentElement, "sheet").headOption match {
      case None => Left("the workbook has no sheets")
      case Some(sheet) =>
        val relId = relationshipId(sheet)
        val target = xml(zip, "xl/_rels/workbook.xml.rels").flatMap { rels =>
          elements(rels.getDocumentElement, "Relationship")
            .find(_.getAttribute("Id") == relId)
            .map(_.getAttribute("Target"))
        }
        val path = target
          .map(t => if (t.startsWith("/")) t.drop(1) else "xl/" + t)
          .getOrElse("xl/worksheets/sheet1.xml")
        Right(SheetRef(sheet.getAttribute("name"), path))
    }

  // Atribūts r:id (vārdtelpa var atšķirties, tāpēc tiek meklēts pēc vārda "id").
  private def relationshipId(element: Element): String = {
    val attributes = element.getAttributes
    (0 until attributes.getLength)
      .map(i => attributes.item(i))
      .find(attribute => Option(attribute.getLocalName).contains("id"))
      .map(_.getNodeValue)
      .getOrElse("")
  }

  // Kopīgās teksta vērtības: <si><t>teksts</t></si> vai <si><r><t>da</t></r><r><t>ļa</t></r></si>.
  private def sharedStrings(document: Document): Vector[String] =
    children(document.getDocumentElement, "si").map(text)

  private def text(item: Element): String =
    childElements(item).map { child =>
      localName(child) match {
        case "t" => child.getTextContent
        case "r" => children(child, "t").map(_.getTextContent).mkString
        case _   => ""
      }
    }.mkString

  // Stilu numuri (cellXfs secībā), kuriem ir datuma formāts.
  private def dateStyleIndexes(styles: Document): Set[Int] = {
    val root = styles.getDocumentElement
    val custom: Map[Int, String] = elements(root, "numFmt").flatMap { format =>
      format.getAttribute("numFmtId").toIntOption.map(id => (id, format.getAttribute("formatCode")))
    }.toMap
    val formats = elements(root, "cellXfs").headOption.map(xfs => children(xfs, "xf")).getOrElse(Vector.empty)
    formats.zipWithIndex.collect {
      case (xf, index) if isDateFormat(xf.getAttribute("numFmtId").toIntOption.getOrElse(0), custom) => index
    }.toSet
  }

  // Iebūvētie datuma formāti ir 14-22; pārējiem tiek pārbaudīts formāta kods.
  private def isDateFormat(id: Int, custom: Map[Int, String]): Boolean =
    custom.get(id) match {
      case Some(code) => isDateCode(code)
      case None       => id >= 14 && id <= 22
    }

  // Lapas rindas; trūkstošās šūnas tiek aizpildītas ar tukšu tekstu.
  private def sheetRows(
      sheet: Document,
      strings: Vector[String],
      dateStyles: Set[Int],
      date1904: Boolean
  ): Vector[(Int, Either[String, Vector[String]])] = {
    val parsed = elements(sheet.getDocumentElement, "row").zipWithIndex.map { case (row, index) =>
      val number = row.getAttribute("r").toIntOption.getOrElse(index + 1)
      val cells = children(row, "c").zipWithIndex.map { case (cell, position) =>
        (columnIndex(cell.getAttribute("r")).getOrElse(position), cellValue(cell, strings, dateStyles, date1904))
      }
      (number, cells.toMap)
    }
    val width = parsed.flatMap { case (_, cells) => cells.keys.map(_ + 1) }.maxOption.getOrElse(0)
    parsed.map { case (number, cells) =>
      (number, Right((0 until width).map(i => cells.getOrElse(i, "")).toVector))
    }
  }

  // Šūnas vērtība tekstā atkarībā no tās tipa (atribūts t).
  private def cellValue(cell: Element, strings: Vector[String], dateStyles: Set[Int], date1904: Boolean): String = {
    val raw = children(cell, "v").headOption.map(_.getTextContent).getOrElse("")
    cell.getAttribute("t") match {
      case "s"              => raw.toIntOption.flatMap(i => strings.lift(i)).getOrElse("")
      case "inlineStr"      => children(cell, "is").headOption.map(text).getOrElse("")
      case "b"              => if (raw == "1") "TRUE" else "FALSE"
      case "str" | "e" | "d" => raw
      case _ =>
        val isDate = cell.getAttribute("s").toIntOption.exists(dateStyles.contains)
        raw.toDoubleOption match {
          case Some(number) if isDate => excelDate(number, date1904)
          case Some(number)           => plainNumber(number)
          case None                   => raw
        }
    }
  }

  // 15 zīmīgie cipari: 3.8376187000000002E-2 -> 0.038376187.
  private def plainNumber(value: Double): String = {
    val text = BigDecimal(value).round(new MathContext(15)).bigDecimal.stripTrailingZeros.toPlainString
    if (text == "-0") "0" else text
  }

  // ----- XML palīgfunkcijas -----

  private def parser(): DocumentBuilder = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    factory.setXIncludeAware(false)
    factory.setExpandEntityReferences(false)
    factory.newDocumentBuilder()
  }

  private def xml(zip: ZipFile, name: String): Option[Document] =
    Option(zip.getEntry(name)).map { entry =>
      Using.resource(zip.getInputStream(entry))(stream => parser().parse(stream))
    }

  private def localName(element: Element): String =
    Option(element.getLocalName).getOrElse(element.getTagName)

  // Visi pēcteči ar norādīto vārdu (jebkurā vārdtelpā).
  private def elements(parent: Element, name: String): Vector[Element] = {
    val list = parent.getElementsByTagNameNS("*", name)
    (0 until list.getLength).map(i => list.item(i)).collect { case e: Element => e }.toVector
  }

  private def childElements(parent: Element): Vector[Element] = {
    val nodes = parent.getChildNodes
    (0 until nodes.getLength).map(i => nodes.item(i)).collect { case e: Element => e }.toVector
  }

  // Tiešie bērni ar norādīto vārdu.
  private def children(parent: Element, name: String): Vector[Element] =
    childElements(parent).filter(child => localName(child) == name)
}
