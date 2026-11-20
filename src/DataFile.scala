package lab2

import java.nio.file.{Files, Path}

/** Datu faila nolasīšanas rezultāts.
  *
  * @param frame   nolasītais datu rāmis
  * @param format  faila formāta apraksts, piem. `Excel (.xlsx), sheet 'Sheet1'`
  * @param skipped izlaisto (nekorekto) rindu apraksti
  */
final case class LoadResult(frame: DataFrame, format: String, skipped: Vector[String])

/** Datu kopas faila nolasīšana neatkarīgi no formāta.
  *
  * Atbalstītie formāti:
  *  - `.xlsx` - Excel darbgrāmata (tāda ir datu kopa Kaggle), skatīt [[Xlsx]];
  *  - `.csv` un citi teksta faili, skatīt [[Csv]].
  *
  * Abos gadījumos faila rindas vispirms tiek pārveidotas par vērtību
  * vektoriem, un pēc tam datu rāmis tiek izveidots vienādi ([[fromRows]]):
  *  - tukšas rindas tiek izlaistas;
  *  - divu līmeņu galvene (kā datu kopas UCI versijā: `TL BASED` /
  *    `USD BASED` virs divām ISE kolonnām) tiek apvienota vienā rindā;
  *  - tukšiem kolonnu nosaukumiem tiek piešķirts nosaukums `column_N`,
  *    atkārtotiem - numurs (Kaggle failā ir divas kolonnas `ISE`, tās kļūst
  *    par `ISE` un `ISE.1`);
  *  - rindas ar nepareizu vērtību skaitu tiek izlaistas un uzskaitītas.
  */
object DataFile {

  /** Nolasa datu failu, izvēloties formātu pēc faila paplašinājuma.
    *
    * @param path faila ceļš
    * @return [[LoadResult]] vai `Left`, ja fails neeksistē, ir mape, ir vecā
    *         Excel formātā (`.xls`) vai to nevar nolasīt
    */
  def read(path: Path): Either[String, LoadResult] = {
    val name = Option(path.getFileName).map(_.toString.toLowerCase).getOrElse("")
    if (!Files.exists(path)) Left(s"file not found: $path")
    else if (Files.isDirectory(path)) Left(s"$path is a folder, not a file")
    else if (name.endsWith(".xls")) Left("the old Excel format (.xls) is not supported - save the file as .xlsx or .csv")
    else if (name.endsWith(".xlsx")) Xlsx.read(path)
    else Csv.read(path)
  }

  /** Izveido datu rāmi no faila rindām.
    *
    * @param rows   rindas: rindas numurs failā un vērtības (vai kļūda, ja rindu
    *               nevarēja sadalīt vērtībās)
    * @param format faila formāta apraksts rezultātam
    * @return [[LoadResult]] vai `Left`, ja failā nav nevienas netukšas rindas
    *         vai galveni nevar nolasīt
    */
  def fromRows(rows: Vector[(Int, Either[String, Vector[String]])], format: String): Either[String, LoadResult] = {
    val nonEmpty = rows
      .map { case (number, cells) => (number, cells.map(_.map(_.trim))) }
      .filter {
        case (_, Right(cells)) => cells.exists(_.nonEmpty)
        case _                 => true
      }
    nonEmpty.headOption match {
      case None                      => Left("the file is empty")
      case Some((_, Left(error)))    => Left(s"cannot read the header: $error")
      case Some((_, Right(header))) =>
        val (subHeader, dataRows) = detectSubHeader(header, nonEmpty.tail)
        val merged  = subHeader.fold(header)(sub => mergeHeader(header, sub))
        val columns = uniqueNames(fillEmptyNames(merged))
        val (frameRows, skipped) = collectRows(columns.size, dataRows)
        Right(LoadResult(DataFrame(columns, frameRows), format, skipped))
    }
  }

  // Divu līmeņu galvene (datu kopas UCI versijā):
  //   ,TL BASED,USD BASED,,,,,,,
  //   date,ISE,ISE,SP,DAX,FTSE,NIKKEI,BOVESPA,EU,EM
  // Ja pirmajā rindā ir tukši vai atkārtoti nosaukumi un nākamajā rindā nav
  // neviena skaitļa, tad abas rindas ir galvene un tiek apvienotas.
  private def detectSubHeader(
      header: Vector[String],
      rows: Vector[(Int, Either[String, Vector[String]])]
  ): (Option[Vector[String]], Vector[(Int, Either[String, Vector[String]])]) = {
    val twoLevelHeader = header.exists(_.isEmpty) || header.distinct.size != header.size
    rows.headOption match {
      case Some((_, Right(cells))) if twoLevelHeader && isTextRow(cells) => (Some(cells), rows.tail)
      case _                                                          => (None, rows)
    }
  }

  // Rinda ir teksta rinda, ja tajā ir vismaz viena vērtība un nav neviena skaitļa.
  private def isTextRow(cells: Vector[String]): Boolean =
    cells.exists(_.nonEmpty) && !cells.exists(cell => DataFrame.toNumber(cell).isDefined)

  // Apvieno galvenes divas rindas: "ISE" + "TL BASED" -> "ISE (TL BASED)".
  // Kolonnu nosaukumi ir tajā rindā, kurā ir mazāk tukšu šūnu; otra rinda
  // ir precizējums.
  private def mergeHeader(first: Vector[String], second: Vector[String]): Vector[String] = {
    val (names, details) =
      if (second.count(_.isEmpty) < first.count(_.isEmpty)) (second, first) else (first, second)
    names.zipAll(details, "", "").map { case (name, detail) =>
      if (name.isEmpty) detail
      else if (detail.isEmpty) name
      else s"$name ($detail)"
    }
  }

  // Tukšam nosaukumam piešķir nosaukumu "column_N".
  private def fillEmptyNames(names: Vector[String]): Vector[String] =
    names.zipWithIndex.map { case (name, index) => if (name.isEmpty) s"column_${index + 1}" else name }

  // Atkārtotiem nosaukumiem pievieno numuru: ISE, ISE.1, ISE.2.
  private def uniqueNames(names: Vector[String]): Vector[String] =
    names
      .foldLeft((Vector.empty[String], Map.empty[String, Int])) { case ((result, seen), name) =>
        val count  = seen.getOrElse(name, 0)
        val unique = if (count == 0) name else s"$name.$count"
        (result :+ unique, seen.updated(name, count + 1))
      }
      ._1

  // Atlasa korektās rindas; pārējās tiek izlaistas ar paskaidrojumu.
  private def collectRows(
      width: Int,
      rows: Vector[(Int, Either[String, Vector[String]])]
  ): (Vector[Vector[String]], Vector[String]) =
    rows.foldLeft((Vector.empty[Vector[String]], Vector.empty[String])) {
      case ((good, skipped), (number, Left(error))) =>
        (good, skipped :+ s"line $number: $error")
      case ((good, skipped), (number, Right(cells))) =>
        if (cells.size == width) (good :+ cells, skipped)
        else (good, skipped :+ s"line $number: ${cells.size} values, expected $width")
    }
}
