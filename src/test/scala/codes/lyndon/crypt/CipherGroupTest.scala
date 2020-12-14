package codes.lyndon.crypt

import org.scalatest.funsuite.AnyFunSuite
import codes.lyndon.crypt.CipherUtils._

class CipherGroupTest extends AnyFunSuite {

  test("testEncode") {
    val group = CipherGroup(
      NoOpCipher,
      CaesarCipher(13),
      SimpleSubstitution(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "QWERTYUIOPASDFGHJKLZXCVBNM"
      )
    )
    val message = "This is a test of the cipher".prepareForCipher()
    val encoded = group.encode(message)

    assert(encoded == "UXCYCYFUKYUWLUXKHCEXKT")
  }

  test("testDecode") {
    val group = CipherGroup(
      NoOpCipher,
      CaesarCipher(13),
      SimpleSubstitution(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "QWERTYUIOPASDFGHJKLZXCVBNM"
      )
    )
    val expected = "This is a test of the cipher".prepareForCipher()
    val encoded = "UXCYCYFUKYUWLUXKHCEXKT"
    val decoded = group.decode(encoded)

    assert(decoded == expected)
  }

  test("Complex") {

    val msg = "This is my secret message that nobody else can read".prepareForCipher()
    val vKey = "The quick brown fox jumps over the lazy dog".prepareForCipher()

    val group = CipherGroup(
      VigenereCipher(vKey),
      SimpleSubstitution(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "QWERTYUIOPASDFGHJKLZXCVBNM"
      ),
      RouteCipher.Horizontal(5, RouteCipher.alphabetPadding),
      SimpleSubstitution(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "ZXCVBNMASDFGHJKLQWERTYUIOP"
      ),
    )

    val encoded = group.encode(msg)
    val decoded = group.decode(encoded)

    println(msg)
    println(encoded)
    println(decoded)

    println(encoded.asGrid(5).mkString("\t"))
  }

}
