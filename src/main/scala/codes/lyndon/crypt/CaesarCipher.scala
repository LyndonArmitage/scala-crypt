package codes.lyndon.crypt
package codes.lyndon.crypt

case class CaesarCipher(
    shiftAmount: Int,
    private val range: Seq[Char] = 'A' to 'Z'
) extends Cipher {

  override def encode(text: String): String = encode(text, shiftAmount)

  override def decode(text: String): String = encode(text, -shiftAmount)

  private[this] def encode(text: String, shift: Int): String = {
    val array = text.toUpperCase.toCharArray

    array.map { c =>
      val pos = range.indexOf(c)
      if (pos != -1) {
        var newPos = pos + shift
        while (newPos >= range.length) {
          newPos = newPos - range.length
        }
        while (newPos < 0) {
          newPos = newPos + range.length
        }
        range(newPos)
      } else {
        c
      }
    }.mkString
  }
}
