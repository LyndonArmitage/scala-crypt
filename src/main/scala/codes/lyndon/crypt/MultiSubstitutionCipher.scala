package codes.lyndon.crypt

case class MultiSubstitutionCipher(
    charMap: Map[Char, Seq[Char]]
) extends Cipher {

  private[this] val badCounts = charMap.values.flatten
    .groupBy(identity)
    .view
    .mapValues(_.size)
    .filter { case (_, i) => i > 1 }

  if (badCounts.nonEmpty) {
    throw MultiSubstitutionCipher.MultipleSubstitutionsForSingleChar(
      badCounts.keys.toSeq
    )
  }

  private[this] val reverseMap = charMap
    .map(_.swap)
    .flatMap {
      case (keys, c) =>
        keys.map((_, c))
    }

  override def encode(text: String): String = {

    // Create a map with counts so we rotate between all keys
    val mapWithCounts = charMap.map {
      case (c, values) =>
        (c, values.map(Entry(_, 0)))
    }

    text.toCharArray.map { c =>
      val entry = mapWithCounts.get(c)
      entry
        .map { e =>
          val min = e.min
          min.count = min.count + 1
          min.c
        }
        .getOrElse(c)
    }.mkString
  }

  override def decode(text: String): String = {
    text.toCharArray.map(c => reverseMap.getOrElse(c, c)).mkString
  }

  private[this] case class Entry(c: Char, var count: Int)
      extends Ordered[Entry] {
    override def compare(that: Entry): Int = count.compareTo(that.count)
  }
}

object MultiSubstitutionCipher {

  def apply(keys: (Char, Seq[Char])*): MultiSubstitutionCipher =
    MultiSubstitutionCipher(Map(keys: _*))

  case class MultipleSubstitutionsForSingleChar(
      chars: Seq[Char]
  ) extends IllegalArgumentException(
        s"Multiple substitutions for: ${chars.mkString(", ")}"
      )
}
