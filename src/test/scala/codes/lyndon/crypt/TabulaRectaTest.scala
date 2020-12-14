package codes.lyndon.crypt
package codes.lyndon.crypt

import org.scalatest.funsuite.AnyFunSuite

class TabulaRectaTest extends AnyFunSuite {

  test("testEncode") {
    ('A' to 'Z').map { c =>
      val encoded = TabulaRecta.encode(c, 'A')
      assert(encoded == c, "Should match when A")
    }
    val examples = Map(
      ('A', 'B') -> 'B',
      ('A', 'C') -> 'C',
      ('A', 'D') -> 'D',
      ('Z', 'Z') -> 'Y',
      ('O', 'S') -> 'G',
    )

    examples.map { case ((c, key), expected) =>
      assert(TabulaRecta.encode(c, key) == expected)
    }
  }

  test("testDecode") {
    ('A' to 'Z').map { c =>
      val encoded = TabulaRecta.decode(c, 'A')
      assert(encoded == c, "Should match when A")
    }
    val examples = Map(
      ('Y', 'Z') -> 'Z',
      ('Z', 'A') -> 'Z',
      ('Z', 'Z') -> 'A',
      ('G', 'D') -> 'D',
      ('C', 'D') -> 'Z'
    )

    examples.map { case ((c, key), expected) =>
      assert(TabulaRecta.decode(c, key) == expected)
    }
  }

}
