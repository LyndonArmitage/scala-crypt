package codes.lyndon.crypt

import org.scalatest.funsuite.AnyFunSuite
import codes.lyndon.crypt.CipherUtils._
import codes.lyndon.crypt.MultiSubstitutionCipher.MultipleSubstitutionsForSingleChar

class MultiSubstitutionCipherTest extends AnyFunSuite {

  test("encode/decode correctly") {

    val cipher = MultiSubstitutionCipher(
      ('T', Seq('H')),
      ('H', Seq('T', 'A')),
      ('I', Seq('P', 'D')),
      ('S', Seq('F', 'E')),
      ('M', Seq('O', 'Q')),
      ('E', Seq('R', 'I')),
      ('A', Seq('M'))
    )
    val msg = "This is my message".prepareForCipher()

    val expected = "HTPFDEOYQRFEMGI"
    val encoded = cipher.encode(msg)
    val decoded = cipher.decode(encoded)

    assert(encoded == expected, "encode did not encode expected")
    assert(decoded == msg, "decode did not work")
  }

  test("fails to create for overlapping entries") {
    val e = intercept[MultipleSubstitutionsForSingleChar] {
      MultiSubstitutionCipher(
        ('A', Seq('B', 'C')),
        ('B', Seq('D', 'C')) // should fail for duplicate
      )
    }
    assert(e.chars == Seq('C'))
  }

}
