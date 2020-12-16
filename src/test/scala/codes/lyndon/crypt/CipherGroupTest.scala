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

    val msg =
      "This is my secret message that nobody else can read".prepareForCipher()
    val vKey = "The quick brown fox jumps over the lazy dog".prepareForCipher()

    val group = CipherGroup(
      ExpandingSubstitutionCipher(
        ('A', "QW"),
        ('B', "AS"),
        ('C', "ZX"),
        ('D', "ER"),
        ('E', "QO"),
        ('F', "CV"),
        ('G', "TY"),
        ('H', "FK"),
        ('I', "NB"),
        ('J', "UI"),
        ('K', "JK"),
        ('L', "BN"),
        ('M', "OP"),
        ('N', "ZI"),
        ('O', "WK"),
        ('P', "FM"),
        ('Q', "RR"),
        ('R', "ED"),
        ('S', "DF"),
        ('T', "LM"),
        ('U', "XB"),
        ('V', "GH"),
        ('W', "MJ"),
        ('X', "MN"),
        ('Y', "TR"),
        ('Z', "PO")
      ),
      VigenereCipher(vKey),
      SimpleSubstitution(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "QWERTYUIOPASDFGHJKLZXCVBNM"
      ),
      RouteCipher.Horizontal(5, RouteCipher.alphabetPadding),
      SimpleSubstitution(
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        "ZXCVBNMASDFGHJKLQWERTYUIOP"
      )
    )

    val encoded = group.encode(msg)
    val decoded = group.decode(encoded)

    println(msg)
    println(encoded)
    println(decoded)

    encoded.prettyPrintCipher(5, 6)

    assert(decoded.startsWith(msg), "Message should be in decoded")
  }

}
