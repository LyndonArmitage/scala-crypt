package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils.StringMethods
import org.scalatest.funsuite.AnyFunSuite

class OneTimePadCipherTest extends AnyFunSuite {

  test("encode test") {
    val msg = "This is my message that should be impossible to decrypt"
      .prepareForCipher()
    val cipher = OneTimePadCipher(msg.length)

    val encoded = cipher.encode(msg)
    val decoded = cipher.decode(encoded)

    println(encoded)
    println(decoded)
    assert(decoded == msg)
  }

}
