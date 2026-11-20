package lab2

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.util.Using

/** Neliels mākslīgs datu rāmis testiem.
  *
  * Vērtības ir vienkārši skaitļi, lai gaidītos rezultātus varētu pārbaudīt
  * ar roku. Tajā ir ietverti robežgadījumi:
  *  - datumi nav sakārtoti, un `9-Jan-09` teksta secībā būtu aiz `10-Jan-09`;
  *  - nekorekts datums `abc`;
  *  - trūkstošas vērtības: `NA` (DAX) un tukša šūna (NIKKEI);
  *  - pozitīvi, negatīvi un nulles skaitļi.
  */
object Samples {

  /** Testa datu kolonnas (tādas pašas kā datu kopā, bet mazāk). */
  val columns: Vector[String] = Vector("date", "ISE", "DAX", "FTSE", "NIKKEI", "EM")

  /** Testa datu rāmis ar 5 rindām. */
  val frame: DataFrame = DataFrame(
    columns,
    Vector(
      Vector("9-Jan-09", "0.01", "0.02", "0.04", "-0.01", "0.03"),
      Vector("10-Jan-09", "-0.02", "0.01", "-0.03", "0.05", "-0.01"),
      Vector("5-Jan-09", "0.03", "-0.04", "0.02", "0", "0.02"),
      Vector("12-Feb-10", "0.00", "NA", "0.01", "", "0.01"),
      Vector("abc", "0.02", "0.03", "0.01", "0.02", "0.00")
    )
  )

  /** Kolonnas vērtības (testu rezultātu salīdzināšanai).
    *
    * @param frame datu rāmis
    * @param name  kolonnas nosaukums
    * @return kolonnas vērtības vai `Left`, ja kolonnas nav
    */
  def column(frame: DataFrame, name: String): Either[String, Vector[String]] =
    frame.requireColumn(name).map(index => frame.rows.map(row => row(index)))

  private val ns = "http://schemas.openxmlformats.org/spreadsheetml/2006/main"

  /** Minimāla Excel faila daļas (līdzīgi datu kopas failam):
    *  - 1. rinda: `TL BASED` un `USD BASED` virs ISE kolonnām (kā UCI versijā);
    *  - 2. rinda: kolonnu nosaukumi (`EM` sadalīts divās teksta daļās);
    *  - datumi kā Excel skaitļi ar datuma formātu (39818 = 5-Jan-09);
    *  - skaitļi zinātniskajā pierakstā un trūkstošas šūnas (C4, D3).
    */
  val xlsxParts: Vector[(String, String)] = Vector(
    "xl/workbook.xml" ->
      (s"""<workbook xmlns="$ns" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">""" +
        """<sheets><sheet name="Sheet1" sheetId="1" r:id="rId1"/></sheets></workbook>"""),
    "xl/_rels/workbook.xml.rels" ->
      ("""<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">""" +
        """<Relationship Id="rId1" Target="worksheets/sheet1.xml" """ +
        """Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet"/></Relationships>"""),
    "xl/sharedStrings.xml" ->
      (s"""<sst xmlns="$ns"><si><t>TL BASED</t></si><si><t>USD BASED</t></si><si><t>date</t></si>""" +
        """<si><t>ISE</t></si><si><r><t>E</t></r><r><t>M</t></r></si></sst>"""),
    "xl/styles.xml" ->
      (s"""<styleSheet xmlns="$ns"><numFmts count="1"><numFmt numFmtId="164" formatCode="[$$-409]d\\-mmm\\-yy;@"/>""" +
        """</numFmts><cellXfs count="2"><xf numFmtId="0"/><xf numFmtId="164"/></cellXfs></styleSheet>"""),
    "xl/worksheets/sheet1.xml" ->
      (s"""<worksheet xmlns="$ns"><sheetData>""" +
        """<row r="1"><c r="B1" t="s"><v>0</v></c><c r="C1" t="s"><v>1</v></c></row>""" +
        """<row r="2"><c r="A2" t="s"><v>2</v></c><c r="B2" t="s"><v>3</v></c>""" +
        """<c r="C2" t="s"><v>3</v></c><c r="D2" t="s"><v>4</v></c></row>""" +
        """<row r="3"><c r="A3" s="1"><v>39818</v></c><c r="B3"><v>0.035753708</v></c>""" +
        """<c r="C3"><v>3.8376187000000002E-2</v></c></row>""" +
        """<row r="4"><c r="A4" s="1"><v>39819</v></c><c r="B4"><v>-2.5399999999999999E-2</v></c>""" +
        """<c r="D4"><v>1.23E-2</v></c></row>""" +
        """</sheetData></worksheet>""")
  )

  /** Ieraksta minimālu Excel failu ([[xlsxParts]]) testiem.
    *
    * @param path faila ceļš
    */
  def writeXlsx(path: Path): Unit =
    Using.resource(new ZipOutputStream(Files.newOutputStream(path))) { zip =>
      xlsxParts.foreach { case (name, content) =>
        zip.putNextEntry(new ZipEntry(name))
        zip.write(content.getBytes(StandardCharsets.UTF_8))
        zip.closeEntry()
      }
    }
}
