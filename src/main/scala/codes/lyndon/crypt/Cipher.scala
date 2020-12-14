package codes.lyndon.crypt
package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils._

trait Cipher {

  final def prepareAndEncode(text: String): String =
    encode(text.prepareForCipher())

  final def prepareAndDecode(text: String): String =
    decode(text.prepareForCipher())

  def encode(text: String): String

  def decode(text: String): String

}
