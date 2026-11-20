package lab2

import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeFormatterBuilder, ResolverStyle}
import java.util.Locale
import scala.util.Try

/** Datumu nolasīšana no teksta.
  *
  * Stambulas biržas datu kopā datums ir formā `5-Jan-09`. Šādus datumus
  * nevar kārtot kā tekstu: `"9-Jan-09"` teksta secībā ir aiz `"10-Jan-09"`,
  * un `"Feb"` ir pirms `"Jan"`. Tāpēc datums vispirms tiek pārveidots par
  * `LocalDate`.
  *
  * Atbalstītie formāti: `5-Jan-09`, `5-Jan-2009`, `2009-01-05`,
  * `05.01.2009`, `1/5/2009` (mēnesis/diena/gads) un `5 Jan 2009`.
  * Neeksistējoši datumi (piem. `31-Feb-09`) tiek noraidīti.
  */
object Dates {

  private val patterns: Vector[String] =
    Vector("d-MMM-uu", "d-MMM-uuuu", "uuuu-MM-dd", "d.M.uuuu", "M/d/uuuu", "d MMM uuuu")

  private val formatters: Vector[DateTimeFormatter] = patterns.map { pattern =>
    new DateTimeFormatterBuilder()
      .parseCaseInsensitive()
      .appendPattern(pattern)
      .toFormatter(Locale.ENGLISH)
      .withResolverStyle(ResolverStyle.STRICT)
  }

  /** Pārveido tekstu datumā, mēģinot visus atbalstītos formātus pēc kārtas.
    *
    * @param text datums teksta formā, piem. `5-Jan-09`
    * @return `Some(datums)` vai `None`, ja teksts nav korekts datums
    */
  def parse(text: String): Option[LocalDate] = {
    val value = text.trim
    formatters.iterator
      .map(formatter => Try(LocalDate.parse(value, formatter)).toOption)
      .collectFirst { case Some(date) => date }
  }

  /** Datumu secība (no agrākā uz vēlāko). */
  val ordering: Ordering[LocalDate] = Ordering.by((date: LocalDate) => date.toEpochDay)
}
