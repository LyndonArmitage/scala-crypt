package codes.lyndon.crypt.solve

import codes.lyndon.crypt.CaesarCipher
import codes.lyndon.crypt.CipherUtils.StringMethods

import java.io.File

object CaesarSolver {

  def solve(text: String): Seq[Solution] =
    (0 to 26).map { shift =>
      val decoded = CaesarCipher(shift).decode(text)
      Solution(decoded, shift)
    }

  def solveLikely(
      solutions: Seq[Solution],
      dictionaryFile: File,
      minWordLength: Int
  ): Seq[SolutionWithWordCount] = {
    val dict = DictionaryHelper(dictionaryFile, minWordLength)
    solutions.map { solution =>
      SolutionWithWordCount(solution, dict.countWords(solution.decoded))
    }.sorted
  }

  def solveLikely(
      text: String,
      dictionaryFile: File,
      minWordLength: Int = 3
  ): Seq[SolutionWithWordCount] =
    solveLikely(solve(text), dictionaryFile, minWordLength)

  case class Solution(decoded: String, shift: Int) extends Ordered[Solution] {
    override def compare(that: Solution): Int = shift.compareTo(that.shift)

    override def toString: String = s"$shift = $decoded"
  }

  case class SolutionWithWordCount(
      solution: Solution,
      wordCount: Long
  ) extends Ordered[SolutionWithWordCount] {
    override def compare(that: SolutionWithWordCount): Int =
      that.wordCount.compareTo(wordCount)

    override def toString: String = s"(word count = $wordCount) = $solution"
  }

  def main(args: Array[String]): Unit = {
    val encoded = CaesarCipher(12).encode(
      "This is my message let's decrypt. I am the most intelligent person ever! A brain the size of a planet!"
        .prepareForCipher()
    )

    val solutions = solve(encoded)
    solveLikely(
      solutions,
      new File("/home/lyndon/google-10000-english.txt"),
      3
    )
    //    solveLikely(solutions, new File("/home/lyndon/words_alpha.txt"))
      .filter(_.wordCount > 0)
      .foreach { sol =>
        println(sol)
      }
  }
}
