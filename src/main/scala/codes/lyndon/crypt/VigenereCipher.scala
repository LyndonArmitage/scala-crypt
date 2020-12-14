package codes.lyndon.crypt
package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils.StringMethods

case class VigenereCipher(
    key: String
) extends Cipher {

  override def encode(text: String): String = encode(text, keyFor(text))

  override def decode(text: String): String = decode(text, keyFor(text))

  private[this] def encode(text: String, key: String): String =
    applyKey(text, key, TabulaRecta.encode)

  private[this] def decode(text: String, key: String): String =
    applyKey(text, key, TabulaRecta.decode)

  private[this] def keyFor(text: String): String =
    key.repeatToMatchLength(text.length)

  private[this] def applyKey(
      text: String,
      key: String,
      func: (Char, Char) => Char
  ): String = {
    if (text.length != key.length)
      throw new IllegalArgumentException(
        s"text(${text.length}) and key(${key.length}) must be same length"
      )
    text.toCharArray
      .zip(key.toCharArray)
      .map {
        case (c, keyC) => func(c, keyC)
      }
      .mkString
  }
}
