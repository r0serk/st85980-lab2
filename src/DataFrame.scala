package lab2

/** Nemainīgs (immutable) datu rāmis: kolonnu nosaukumi un rindas.
  *
  * Katra rinda ir vērtību vektors tādā pašā secībā kā kolonnas. Vērtības tiek
  * glabātas kā teksts tieši tā, kā tās bija failā, tāpēc, saglabājot datu
  * rāmi atpakaļ CSV failā, tās nemainās. Skaitliskās operācijas teksta
  * vērtību pārveido skaitlī tikai tad, kad tas ir vajadzīgs
  * ([[DataFrame.toNumber]]).
  *
  * Visas operācijas atgriež jaunu datu rāmi un nemaina esošo
  * (funkcionālās programmēšanas princips). Operācijas, kas var neizdoties
  * (piem. kolonna nav atrasta), atgriež `Either[String, DataFrame]`.
  *
  * @param columns kolonnu nosaukumi
  * @param rows    rindas; katras rindas garums ir vienāds ar kolonnu skaitu
  */
final case class DataFrame(columns: Vector[String], rows: Vector[Vector[String]]) {

  /** Rindu skaits. */
  def rowCount: Int = rows.size

  /** Kolonnu skaits. */
  def columnCount: Int = columns.size

  /** Atrod kolonnas indeksu pēc nosaukuma.
    *
    * Vispirms tiek meklēta precīza sakritība, pēc tam - neņemot vērā lielos
    * un mazos burtus (`em` atrod kolonnu `EM`).
    *
    * @param name kolonnas nosaukums
    * @return `Some(indekss)` (sākot no 0) vai `None`
    */
  def columnIndex(name: String): Option[Int] = {
    val exact = columns.indexOf(name)
    if (exact >= 0) Some(exact)
    else {
      val ignoringCase = columns.indexWhere(_.equalsIgnoreCase(name))
      if (ignoringCase >= 0) Some(ignoringCase) else None
    }
  }

  /** Tas pats, kas [[columnIndex]], bet ar kļūdas paziņojumu, ja kolonnas nav.
    *
    * @param name kolonnas nosaukums
    * @return `Right(indekss)` vai `Left` ar pieejamo kolonnu sarakstu
    */
  def requireColumn(name: String): Either[String, Int] =
    columnIndex(name).toRight(s"column '$name' not found (columns: ${columns.mkString(", ")})")

  /** Atgriež pirmās `n` rindas.
    *
    * @param n rindu skaits; negatīvs skaitlis tiek uzskatīts par 0
    * @return datu rāmis ar ne vairāk kā `n` rindām
    */
  def head(n: Int): DataFrame = copy(rows = rows.take(math.max(n, 0)))

  /** Noņem kolonnu.
    *
    * @param name noņemamās kolonnas nosaukums
    * @return jauns datu rāmis bez šīs kolonnas vai `Left`, ja kolonnas nav
    */
  def dropColumn(name: String): Either[String, DataFrame] =
    requireColumn(name).map { index =>
      DataFrame(columns.patch(index, Nil, 1), rows.map(_.patch(index, Nil, 1)))
    }

  /** Pievieno jaunu kolonnu datu rāmja beigās.
    *
    * @param name   jaunās kolonnas nosaukums (nedrīkst jau eksistēt)
    * @param values vērtības katrai rindai (tik pat daudz, cik rindu)
    * @return jauns datu rāmis vai `Left`, ja nosaukums aizņemts vai vērtību
    *         skaits nesakrīt ar rindu skaitu
    */
  def withColumn(name: String, values: Vector[String]): Either[String, DataFrame] =
    if (columnIndex(name).isDefined) Left(s"column '$name' already exists")
    else if (values.size != rows.size) Left(s"${values.size} values for ${rows.size} rows")
    else Right(DataFrame(columns :+ name, rows.zip(values).map { case (row, value) => row :+ value }))

  /** Kolonnas vērtības kā skaitļi.
    *
    * @param index kolonnas indekss (sākot no 0)
    * @return katrai rindai `Some(skaitlis)` vai `None`, ja vērtība nav skaitlis
    */
  def numbers(index: Int): Vector[Option[Double]] =
    rows.map(row => DataFrame.toNumber(row(index)))

  /** Sakārto rindas pēc atslēgas.
    *
    * Rindas, kurām atslēgu nevar noteikt (`None`, piem. tukša vai nekorekta
    * vērtība), tiek liktas beigās to sākotnējā secībā. Kārtošana ir stabila:
    * rindas ar vienādu atslēgu saglabā savu secību.
    *
    * @param key        funkcija, kas no rindas iegūst atslēgu
    * @param ordering   atslēgu secība
    * @param descending `true` - dilstošā secībā, `false` - augošā
    * @return jauns, sakārtots datu rāmis
    *
    * Sarežģītība: O(n log n).
    */
  def sortRowsBy[K](key: Vector[String] => Option[K], ordering: Ordering[K], descending: Boolean): DataFrame = {
    val keyed      = rows.map(row => (key(row), row))
    val withKey    = keyed.collect { case (Some(value), row) => (value, row) }
    val withoutKey = keyed.collect { case (None, row) => row }
    val order      = if (descending) ordering.reverse else ordering
    copy(rows = withKey.sortBy(_._1)(order).map(_._2) ++ withoutKey)
  }

  /** Sagatavo datu rāmi izdrukai tabulas veidā (līdzīgi kā Spark `show()`).
    *
    * @param limit cik rindas rādīt
    * @return tabula kā teksts
    */
  def show(limit: Int): String = {
    val shown  = rows.take(math.max(limit, 0))
    val widths = columns.indices.map { i =>
      (columns(i).length +: shown.map(row => row.lift(i).getOrElse("").length)).max
    }.toVector
    val border = widths.map(width => "-" * width).mkString("+", "+", "+")
    def line(cells: Vector[String]): String =
      widths.indices.map { i =>
        val cell = cells.lift(i).getOrElse("")
        " " * (widths(i) - cell.length) + cell
      }.mkString("|", "|", "|")
    val footer =
      if (rows.size > shown.size) Vector(s"only showing top ${shown.size} of ${rows.size} rows")
      else Vector(s"${rows.size} rows")
    (Vector(border, line(columns), border) ++ shown.map(line) ++ Vector(border) ++ footer).mkString("\n")
  }
}

/** Palīgfunkcijas datu rāmja vērtībām. */
object DataFrame {

  /** Pārveido šūnas tekstu skaitlī.
    *
    * @param cell šūnas vērtība, piem. `0.0357`, `-1.5e-3`
    * @return `Some(skaitlis)` vai `None` tukšai vai neskaitliskai vērtībai
    *         (piem. `""`, `NA`, `abc`, `NaN`)
    */
  def toNumber(cell: String): Option[Double] =
    cell.trim.toDoubleOption.filter(value => !value.isNaN && !value.isInfinite)
}
