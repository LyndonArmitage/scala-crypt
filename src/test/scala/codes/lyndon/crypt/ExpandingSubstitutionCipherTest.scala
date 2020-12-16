package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils.StringMethods
import org.scalatest.funsuite.AnyFunSuite

class ExpandingSubstitutionCipherTest extends AnyFunSuite {

  test("encodes properly") {
    val msg = "This is a test".prepareForCipher()
    val cipher = ExpandingSubstitutionCipher(
      ('T', "AB"),
      ('H', "CD"),
      ('I', "EF"),
      ('S', "GH"),
      ('A', "IJ"),
      ('E', "KL")
    )

    val encoded = cipher.encode(msg)
    assert(encoded == "ABCDEFGHEFGHIJABKLGHAB")
  }

  test("decodes properly") {
    val msg = "This is a test".prepareForCipher()
    val cipher = ExpandingSubstitutionCipher(
      ('T', "AB"),
      ('H', "CD"),
      ('I', "EF"),
      ('S', "GH"),
      ('A', "IJ"),
      ('E', "KL")
    )

    val decoded = cipher.decode("ABCDEFGHEFGHIJABKLGHAB")
    assert(decoded == msg)
  }

  test("Fails for multiples") {

    assertThrows[IllegalArgumentException] {
      ExpandingSubstitutionCipher(
        ('A', "AB"),
        ('C', "DE"),
        ('B', "AB")
      )
    }
  }
}
