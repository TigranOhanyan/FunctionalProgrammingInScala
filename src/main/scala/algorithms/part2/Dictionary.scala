package algorithms.part2

trait Dictionary[T] {

  def insert(v: T): Unit

  def delete(v: T): Unit

  def search(predicate: T => Boolean): T

  def extremum(predicate: (T, T) => Boolean): T
}
