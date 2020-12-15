package codes.lyndon.crypt.solve

import codes.lyndon.crypt.CipherUtils.StringMethods

import java.io.File

case class DictionaryHelper(words: Seq[String]) {

  def countWords(string: String): Long = {
    words.map{ word =>
      var count = 0
      var lastIndex = word.length * -1
      do {
        lastIndex = string.indexOf(word, lastIndex + word.length)
        if (lastIndex != -1) count = count + 1
      } while (lastIndex != -1)
      count.toLong
    }.sum
  }

}

object DictionaryHelper {

  def apply(file: File, minLength: Int = 2): DictionaryHelper = {
    val source = scala.io.Source.fromFile(file)
    val words =
      try {
        source
          .getLines()
          .map(_.prepareForCipher())
          .filter(s => s.nonEmpty && s.length > minLength)
          .toSeq
      } finally {
        source.close()
      }

    DictionaryHelper(words)
  }

}
