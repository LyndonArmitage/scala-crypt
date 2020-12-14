package codes.lyndon.crypt

import codes.lyndon.crypt.CipherUtils.StringMethods
import org.scalatest.funsuite.AnyFunSuite

class CipherUtilsTest extends AnyFunSuite {

  test("removePunctuation") {
    val map = Map(
      "This is a test" -> "Thisisatest",
      ",.\"'&" -> ""
    )
    map.foreach {
      case (str, expected) => assert(str.removePunctuation() == expected)
    }
  }

  test("prepareForCipher") {
    val map = Map(
      "This is a test" -> "THISISATEST",
      ",.\"'&" -> "",
      "foo bar, goop" -> "FOOBARGOOP"
    )
    map.foreach {
      case (str, expected) => assert(str.prepareForCipher() == expected)
    }
  }

  test("repeatToMatchLength") {
    val original = "KEY"
    val map = Map(
      1 -> "K",
      2 -> "KE",
      3 -> "KEY",
      4 -> "KEYK",
      5 -> "KEYKE",
      6 -> "KEYKEY",
      7 -> "KEYKEYK"
    )
    map.foreach {
      case (length, expected) =>
        assert(original.repeatToMatchLength(length) == expected)
    }
  }

  test("grid") {
    val text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val gridSize = 3
    val expected = Seq(
      "ABC",
      "DEF",
      "GHI",
      "JKL",
      "MNO",
      "PQR",
      "STU",
      "VWX",
      "YZ"
    )

    val grid = text.asGrid(gridSize)

    assert(grid == expected)
  }

  test("grid with padding") {
    val text = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val gridSize = 3
    val expected = Seq(
      "ABC",
      "DEF",
      "GHI",
      "JKL",
      "MNO",
      "PQR",
      "STU",
      "VWX",
      "YZ?"
    )

    val grid = text.asGrid(gridSize, i => Some('?'))

    assert(grid == expected)
  }

  test("prettyPrint") {
    val text = "This is my test message for writing out data"
      .prepareForCipher()
    text.prettyPrintCipher(2, 6)
  }

}
