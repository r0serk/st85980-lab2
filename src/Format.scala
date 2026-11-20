package lab2

/** Skaitļu formatēšana izvadei un CSV failam.
  *
  * Datu kopā vērtības ir dienas izmaiņas ar 9 cipariem aiz komata
  * (piem. `0.035753708`). Aprēķinātās vērtības (piem. vidējā vērtība)
  * tiek noapaļotas līdz 10 cipariem aiz komata, un nevajadzīgās nulles
  * beigās tiek noņemtas, lai izvade būtu salasāma.
  */
object Format {

  /** Cik ciparus aiz komata saglabāt aprēķinātajām vērtībām. */
  val decimals: Int = 10

  /** Pārveido skaitli tekstā bez zinātniskā pieraksta.
    *
    * @param value skaitlis
    * @return skaitlis kā teksts, piem. `0.03`, `-0.0125`, `0`;
    *         `NaN` un bezgalība tiek atgriezti kā `NaN` / `Infinity`
    */
  def number(value: Double): String =
    if (value.isNaN || value.isInfinite) value.toString
    else {
      val text = BigDecimal(value)
        .setScale(decimals, BigDecimal.RoundingMode.HALF_UP)
        .bigDecimal
        .stripTrailingZeros
        .toPlainString
      if (text == "-0") "0" else text
    }
}
