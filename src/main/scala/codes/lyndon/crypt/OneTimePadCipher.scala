package codes.lyndon.crypt

import java.security.SecureRandom
import scala.util.Random

case class OneTimePadCipher(
    private val pad: String
) extends Cipher {

  private var used = false
  def isUsed: Boolean = used

  override def encode(text: String): String = {
    if (text.length > pad.length)
      throw new IllegalArgumentException("text cannot be shorter than pad")
    used = true
    text.zip(pad).map { case (c, key) => encodeChar(c, key) }.mkString
  }

  override def decode(text: String): String = {
    if (text.length > pad.length)
      throw new IllegalArgumentException("text cannot be shorter than pad")
    text.zip(pad).map { case (c, key) => decodeChar(c, key) }.mkString
  }

  private[this] val alphabet = 'A' to 'Z'

  private[this] def encodeChar(
      c: Char,
      key: Char
  ): Char = encodeDecodeChar(c, key, decode = false)

  private[this] def decodeChar(
      c: Char,
      key: Char
  ): Char = encodeDecodeChar(c, key, decode = true)

  private[this] def encodeDecodeChar(
      c: Char,
      key: Char,
      decode: Boolean
  ): Char = {
    val cIndex = alphabet.indexOf(c)
    val keyIndex = alphabet.indexOf(key)
    val value = if (decode) {
      val subbed = cIndex - keyIndex
      if (subbed < 0) {
        alphabet.length + subbed
      } else {
        subbed
      }
    } else {
      cIndex + keyIndex
    }
    alphabet(value % alphabet.length)
  }

}

object OneTimePadCipher {
  def apply(
      length: Int,
      rand: Random = Random.javaRandomToRandom(new SecureRandom())
  ): OneTimePadCipher =
    OneTimePadCipher(generatePad(length, rand))

  def generatePad(
      length: Int,
      rand: Random = Random.javaRandomToRandom(new SecureRandom())
  ): String =
    Array
      .fill(length)(rand.between('A', 'Z' + 1))
      .map(_.toChar)
      .mkString

  def encode(text: String): (String, String) = {
    val pad = generatePad(text.length)
    (OneTimePadCipher(pad).encode(text), pad)
  }
}
