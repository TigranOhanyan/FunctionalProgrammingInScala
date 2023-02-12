package algorithms.part2

sealed class DictionaryException(message: String) extends RuntimeException(message)

case object Overflow extends DictionaryException("Dictionary Overflow")
case object Underflow extends DictionaryException("Dictionary Underflow")
case object NotSupported extends DictionaryException("Operation not supported")