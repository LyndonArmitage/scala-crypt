package codes.lyndon.crypt

import scala.collection.mutable

object TabulaRecta {

  private[this] val alphabet: Array[Char] = ('A' to 'Z').toArray

  private val rectaMap: Map[Char, Map[Char, Char]] =
    alphabet.map { c => (c, alphabet.zip(offsetAlphabet(c)).toMap) }.toMap

  private val rectaMapArray: Map[Char, Array[Char]] =
    alphabet.map { c => (c, offsetAlphabet(c)) }.toMap


  def encode(c: Char, key: Char): Char = {
    val cPos = alphabet.indexOf(c)
    val keyPos = alphabet.indexOf(key)
    if (cPos == -1 || keyPos == -1) return c
    rectaMap(key)(c)
  }

  def decode(c: Char, key: Char): Char = {
    val cPos = alphabet.indexOf(c)
    val keyPos = alphabet.indexOf(key)
    if (cPos == -1 || keyPos == -1) return c
    alphabet(rectaMapArray(key).indexOf(c))
  }

  private def offsetAlphabet(key: Char): Array[Char] = {
    if (key == 'A') return alphabet
    if (!alphabet.contains(key))
      throw new IllegalArgumentException("key must be a letter")
    val deque = mutable.ArrayDeque(alphabet: _*)
    val removed = deque.removeHeadWhile(c => c != key)
    deque.appendAll(removed)
    deque.toArray
  }
}
