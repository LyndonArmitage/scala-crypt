package codes.lyndon.crypt

case class ExpandingSubstitutionCipher(
    size: Int,
    charMap: Map[Char, Array[Char]]
) extends Cipher {

  if (charMap.values.map(_.length).exists(_ != size)) {
    throw new IllegalArgumentException(
      s"All replacements must be of length: $size"
    )
  }

  charMap.values
    .map(_.mkString)
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .filter(_._2 > 1)
    .foreach {
      case (str, i) =>
        throw new IllegalArgumentException(
          s"Multiple entries $i with value: $str"
        )
    }

  // take each character and encode it into n characters

  private[this] val reverseMap =
    charMap.map(_.swap).map { case (chars, c) => (chars.mkString, c) }

  override def encode(text: String): String =
    text.map(c => charMap(c)).flatten.mkString

  override def decode(text: String): String =
    text.toCharArray
      .grouped(size)
      .map(_.mkString)
      .filter(_.length == size)
      .map { str =>
        try {
          reverseMap(str)
        } catch {
          case e: Exception =>
            throw new IllegalArgumentException(s"Could not decode: $str", e)
        }
      }
      .mkString

}

object ExpandingSubstitutionCipher {

  def apply(
      size: Int,
      keys: (Char, String)*
  ): ExpandingSubstitutionCipher = {
    val chars = keys.map { case (c, str) => (c, str.toCharArray) }
    ExpandingSubstitutionCipher(size, Map(chars: _*))
  }

  def apply(keys: (Char, String)*): ExpandingSubstitutionCipher =
    apply(keys.map(_._2.length).max, keys: _*)

}
