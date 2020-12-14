package codes.lyndon.crypt
package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class RouteCipher(
    gridWidth: Int,
    routingFunction: Seq[String] => String,
    reverseRoutingFunction: String => Seq[String],
    paddingFunction: Int => Option[Char] = _ => None
) extends Cipher {
  override def encode(text: String): String = {
    val grid = text.asGrid(gridWidth, paddingFunction)
    routingFunction(grid)
  }

  override def decode(text: String): String = {
    val reversed = reverseRoutingFunction(text)
    reversed.flatten.mkString
  }
}

object RouteCipher {

  def Horizontal(
      gridWidth: Int,
      paddingFunction: Int => Char = _ => '?'
  ): RouteCipher = {
    val routing = (rows: Seq[String]) => {
      val builder = new StringBuilder(rows.length * gridWidth)
      val asBuffers =
        rows.map(_.toCharArray()).map(mutable.ArrayDeque[Char](_: _*))
      var allEmpty = false
      var workingBuffer = 0
      while (!allEmpty) {
        val buffer = asBuffers(workingBuffer)
        val head = buffer.removeHeadOption()
        head.foreach(builder.append)
        if (head.isEmpty) {
          allEmpty = true
        } else {
          workingBuffer = workingBuffer + 1
          if (workingBuffer >= asBuffers.length) workingBuffer = 0
        }
      }

      builder.mkString
    }

    val reverse = (encoded: String) => {
      val rowSize = encoded.length / gridWidth

      val buffers = List.fill(rowSize)(ListBuffer.empty[Char])
      encoded.zipWithIndex.foreach { case (c, i) =>
        val buffer = buffers(i % rowSize)
        buffer.addOne(c)
      }

      buffers.map(_.mkString)
    }

    RouteCipher(gridWidth, routing, reverse, i => Some(paddingFunction(i)))
  }

  private[this] val alphabet = ('A' to 'Z')

  def alphabetPadding: Int => Char = { i =>
    alphabet(i / 26)
  }

  def main(args: Array[String]): Unit = {
    val msg = "THISISTEXT"
    val width = 3
    val cipher = RouteCipher.Horizontal(width)
    val encoded = cipher.encode(msg)

    msg.asGrid(3, _ => Some('?')).foreach(println)
    println(encoded)
  }
}
