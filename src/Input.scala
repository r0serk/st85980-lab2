package lab2

import scala.io.StdIn

/** Droša lietotāja ievades nolasīšana un pārbaude.
  *
  * Katra parsēšanas funkcija atgriež `Either[String, A]`:
  *  - `Right(x)` - ievade ir korekta, `x` ir pārveidotā vērtība;
  *  - `Left(err)` - ievade ir nekorekta, `err` ir paziņojums lietotājam.
  *
  * Apstrādātie robežgadījumi: tukša ievade, burti vai citi simboli skaitļa
  * vietā, decimāldaļskaitlis vesela skaitļa vietā, skaitlis ārpus tipa `Int`
  * diapazona un ievades beigas (EOF, piem. Ctrl+D vai Ctrl+Z).
  */
object Input {

  /** Pārveido teksta virkni veselā skaitlī (`Int`).
    *
    * Atļautais formāts: neobligāta zīme `+` vai `-` un vismaz viens cipars.
    * Atstarpes sākumā un beigās tiek ignorētas.
    *
    * @param raw lietotāja ievadītā virkne
    * @return `Right(n)` korektam skaitlim vai `Left` ar kļūdas aprakstu
    */
  def parseInt(raw: String): Either[String, Int] = {
    val text = raw.trim
    if (text.isEmpty) Left("empty input - a number was expected")
    else if (text.exists(_.isLetter)) Left(s"'$text' is not a number (letters are not allowed)")
    else if (text.contains('.') && text.forall(c => c.isDigit || "+-.".contains(c)))
      Left(s"'$text' is not a whole number")
    else if (!text.matches("[+-]?[0-9]+")) Left(s"'$text' is not a valid integer")
    else {
      val value = BigInt(text)
      if (value < BigInt(Int.MinValue) || value > BigInt(Int.MaxValue))
        Left(s"'$text' is outside the Int range (${Int.MinValue} .. ${Int.MaxValue})")
      else Right(value.toInt)
    }
  }

  /** Nolasa vienu rindu no standarta ievades.
    *
    * @return `Some(rinda)` vai `None`, ja ievade ir beigusies (EOF)
    */
  def readLineSafe(): Option[String] = Option(StdIn.readLine())

  /** Izdrukā uzvedni un nolasa ievadi, līdz tā ir korekta.
    *
    * Nekorektas ievades gadījumā tiek izdrukāts kļūdas paziņojums un ievade
    * tiek pieprasīta vēlreiz, tāpēc programma neapstājas kļūdas dēļ.
    *
    * @param prompt uzvednes teksts
    * @param parser funkcija, kas pārbauda un pārveido ievadi
    * @return `Some(vērtība)` vai `None`, ja ievade beidzās (EOF)
    */
  def promptUntilValid[A](prompt: String, parser: String => Either[String, A]): Option[A] = {
    @annotation.tailrec
    def loop(): Option[A] = {
      print(prompt)
      Console.flush()
      readLineSafe() match {
        case None => None
        case Some(line) =>
          parser(line) match {
            case Right(value) => Some(value)
            case Left(error) =>
              println(s"  Error: $error. Please try again.")
              loop()
          }
      }
    }
    loop()
  }

  /** Uzdod jautājumu ar atbildi "jā" vai "nē" (`y`, `yes`, `n`, `no`).
    *
    * @param prompt jautājuma teksts
    * @return `true`, ja atbilde ir "jā"; `false`, ja "nē" vai ievade beigusies
    */
  def askYesNo(prompt: String): Boolean =
    promptUntilValid(prompt, parseYesNo).getOrElse(false)

  /** Pārbauda atbildi "jā"/"nē".
    *
    * @param raw lietotāja ievade
    * @return `Right(true)` / `Right(false)` vai `Left` nekorektai atbildei
    */
  def parseYesNo(raw: String): Either[String, Boolean] =
    raw.trim.toLowerCase match {
      case ""           => Left("empty answer (type y or n)")
      case "y" | "yes"  => Right(true)
      case "n" | "no"   => Right(false)
      case other        => Left(s"'$other' is not a valid answer (type y or n)")
    }

  /** Gaida, kamēr lietotājs nospiež Enter, lai atgrieztos galvenajā izvēlnē. */
  def waitForEnter(): Unit = {
    print("\nPress Enter to return to the main menu...")
    Console.flush()
    readLineSafe()
    println()
  }
}
