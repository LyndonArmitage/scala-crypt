package codes.lyndon.crypt

object NoOpCipher extends Cipher {
  override def encode(text: String): String = text

  override def decode(text: String): String = text
}
