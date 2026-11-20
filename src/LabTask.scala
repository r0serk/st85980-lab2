package lab2

import java.nio.file.Path

/** Nolasītā datu kopa, ar kuru strādā visi uzdevumi.
  *
  * @param path   CSV faila ceļš
  * @param loaded nolasīšanas rezultāts (datu rāmis, atdalītājs, izlaistās rindas)
  */
final case class Dataset(path: Path, loaded: LoadResult) {

  /** Nolasītais datu rāmis. */
  def frame: DataFrame = loaded.frame
}

/** Kopīgā saskarne visiem uzdevumiem (2.-10.).
  *
  * Galvenā izvēlne katram uzdevumam vispirms izpilda testus ([[tests]]) ar
  * nelielu mākslīgu datu rāmi ([[Samples.frame]]) un pēc tam parāda
  * rezultātu ar īsto datu kopu ([[run]]).
  */
trait LabTask {

  /** Uzdevuma numurs laboratorijas darba aprakstā. */
  def number: Int

  /** Īss uzdevuma nosaukums izvēlnei. */
  def title: String

  /** Uzdevuma testi, arī robežgadījumi. */
  def tests: Vector[TestResult]

  /** Izpilda uzdevumu ar īsto datu kopu un izdrukā rezultātu.
    *
    * @param data nolasītā datu kopa
    */
  def run(data: Dataset): Unit
}
