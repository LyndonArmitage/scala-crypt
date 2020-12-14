package codes.lyndon.crypt

import org.scalatest.funsuite.AnyFunSuite
import codes.lyndon.crypt.CipherUtils.StringMethods

class VigenereCipherTest extends AnyFunSuite {

  test("testEncode") {
    val key = "MY KEY IS BETTER THAN YOURS".prepareForCipher()
    val message = "This is a message".prepareForCipher()
    val cipher = VigenereCipher(key)
    val encoded = cipher.encode(message)
    println(encoded)
    assert(encoded == "FFSWGASNILLEXX")
  }

  test("testDecode") {
    val key = "MY KEY IS BETTER THAN YOURS".prepareForCipher()
    val message = "This is a message".prepareForCipher()
    val cipher = VigenereCipher(key)
    val encoded = "FFSWGASNILLEXX"
    val decoded = cipher.decode(encoded)
    println(decoded)
    assert(decoded == message)

  }

}
