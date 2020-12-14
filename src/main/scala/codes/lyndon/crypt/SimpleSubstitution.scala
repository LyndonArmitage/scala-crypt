package codes.lyndon.crypt

case class SimpleSubstitution(
    charMap: Map[Char, Char]
) extends Cipher {

  private[this] val reverseMap = charMap.map(_.swap)

  override def encode(text: String): String = encode(text, charMap)

  override def decode(text: String): String = encode(text, reverseMap)

  private[this] def encode(text: String, map: Map[Char, Char]): String =
    text.toCharArray.map(c => map.getOrElse(c, c)).mkString
}

object SimpleSubstitution {

  def apply(keys: (Char, Char)*): SimpleSubstitution =
    SimpleSubstitution(Map(keys: _*))

  def apply(keys: String, replacements: String): SimpleSubstitution = {
    if (keys.length != replacements.length) {
      throw new IllegalArgumentException(
        "keys and replacements must be same length"
      )
    }
    SimpleSubstitution(keys.toCharArray.zip(replacements.toCharArray).toMap)
  }

}
