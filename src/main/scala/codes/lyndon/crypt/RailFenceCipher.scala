package codes.lyndon.crypt
package codes.lyndon.crypt

import scala.collection.mutable
import codes.lyndon.crypt.CipherUtils._

case class RailFenceCipher(diagonalLength: Int) extends Cipher {

  // TODO: Fix this https://en.wikipedia.org/wiki/Rail_fence_cipher
  override def encode(text: String): String = {
    val buckets = List.fill(diagonalLength)(mutable.ListBuffer.empty[Char])

    for ( (c, index) <- text.zipWithIndex) {
      val bucket = index % diagonalLength
      buckets(bucket).addOne(c)
    }

    buckets.flatMap(_.toSeq).mkString
  }

  override def decode(text: String): String = {

    val textLen = text.length

    val buckets = List.fill(diagonalLength)(mutable.ListBuffer.empty[Char])
    for ( (c, index) <- text.zipWithIndex) {
      val bucket = index % diagonalLength
      buckets(bucket).addOne(c)
    }

    val builder = new mutable.StringBuilder(text.length)
    var index = 0
    while(index < text.length) {
      val bucketIndex = index % diagonalLength
      val c = buckets(bucketIndex).remove(0)
      builder.append(c)
      index = index + 1
    }

    builder.mkString
  }

}

object RailFenceCipher {
  def main(args: Array[String]): Unit = {
    val text = "WE ARE DISCOVERED FLEE AT ONCE".prepareForCipher()

    val encoded = RailFenceCipher(3).encode(text)

    println(encoded)
    println(RailFenceCipher(3).decode(encoded))
  }
}
