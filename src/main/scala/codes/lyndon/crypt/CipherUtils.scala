package codes.lyndon.crypt

import scala.collection.mutable.ListBuffer

object CipherUtils {

  implicit class StringMethods(string: String) {

    def prepareForCipher(): String =
      string.trim.toUpperCase
        .removePunctuation()

    def removePunctuation(): String =
      string.toCharArray.filter(c => c.isLetterOrDigit).mkString

    def repeatToMatchLength(matchLength: Int): String = {
      if (string.length >= matchLength) {
        // key is long enough for whole text
        string.substring(0, matchLength)
      } else {
        // key needs to be repeated for text
        val mult = (matchLength / string.length) + 1
        val repeat = (string * mult)
        repeat.substring(0, matchLength)
      }
    }

    def asGrid(
        width: Int,
        paddingFunction: Int => Option[Char] = _ => None
    ): IndexedSeq[String] = {
      val buffer = ListBuffer.empty[String]
      val builder = new StringBuilder(width)
      for ((c, index) <- string.toCharArray.zipWithIndex) {
        builder.append(c)
        if (((index + 1) - (buffer.size * width)) / width > 0) {
          buffer.addOne(builder.mkString)
          builder.clear()
        }
      }
      if (builder.nonEmpty) {
        if (builder.size < width) {
          (builder.size until width)
            .map(paddingFunction)
            .foreach(_.map(builder.append))
        }
        buffer.addOne(builder.mkString)
      }
      buffer.toIndexedSeq
    }

    def prettyPrintCipher(
        groupSize: Int,
        groupsPerLine: Int,
        spacing: String = "\t",
        paddingFunction: Int => Option[Char] = _ => None
    ): Unit = {
      val groups = string.asGrid(groupSize, paddingFunction)
      groups.zipWithIndex.foreach {
        case (str, i) =>
          print(str)
          if ((i + 1) % groupsPerLine == 0) {
            println()
          } else {
            print(spacing)
          }
      }
    }

  }

}
