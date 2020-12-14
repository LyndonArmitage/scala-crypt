package codes.lyndon.crypt
package codes.lyndon.crypt

import org.scalatest.funsuite.AnyFunSuite

class CaesarCipherTest extends AnyFunSuite {

  test("encode") {
    assert(CaesarCipher(0).encode("TEXT") == "TEXT")
    assert(CaesarCipher(1).encode("TEXT") == "UFYU")
    assert(CaesarCipher(13).encode("TEXT") == "GRKG")
    assert(CaesarCipher(26).encode("TEXT") == "TEXT")
    assert(CaesarCipher(25).encode("TEXT") == "SDWS")
    assert(CaesarCipher(-1).encode("TEXT") == "SDWS")
    assert(CaesarCipher(52).encode("TEXT") == "TEXT")
    assert(CaesarCipher(53).encode("TEXT") == "UFYU")
  }

  test("decode") {
    assert(CaesarCipher(0).decode("TEXT") == "TEXT")
    assert(CaesarCipher(13).decode("GRKG") == "TEXT")
    assert(CaesarCipher(53).decode("UFYU") == "TEXT")
    assert(CaesarCipher(25).decode("SDWS") == "TEXT")
    assert(CaesarCipher(-1).decode("SDWS") == "TEXT")
  }

}
