package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils._
import org.scalatest.funsuite.AnyFunSuite

class RouteCipherTest extends AnyFunSuite {

  test("basic row to column") {
    val width = 3
    val padding: Int => Char = _ => '?'
    val cipher = RouteCipher.Horizontal(width, padding)

    val msg = "THISISTEXT"
    val expected = "TSTTHIE?ISX?"
    val encoded = cipher.encode(msg)
    val decoded = cipher.decode(encoded)
    val grid = msg.asGrid(width, i => Some(padding(i)))

    println("Message:")
    println(msg)
    println("Grid:")
    grid.foreach(println)
    println()

    println("Expected:")
    println(expected)
    println("Encoded:")
    println(encoded)
    println("Decoded:")
    println(decoded)

    assert(encoded == expected)
    assert(decoded == s"$msg??")
  }

}
