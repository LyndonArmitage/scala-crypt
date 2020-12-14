package codes.lyndon.crypt

case class CipherGroup(
    ciphers: Cipher*
) extends Cipher {
  override def encode(text: String): String = {
    ciphers.foldLeft(text)((t, cipher) => cipher.encode(t))
  }

  override def decode(text: String): String = {
    ciphers.foldRight(text)((cipher, t) => cipher.decode(t))
  }
}

object CipherGroup {
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      sys.error("Missing argument")
      return
    }
    val group = CipherGroup(
      CaesarCipher(5),
      SimpleSubstitution(
        '1' -> '2',
        '2' -> '1'
      )
    )
    import CipherUtils.StringMethods
    val text = args(0).removePunctuation()
    val encoded = group.encode(text)
    val decoded = group.decode(encoded)

    println(decoded)
    println(encoded)
  }
}
