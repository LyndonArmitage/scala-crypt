package codes.lyndon.crypt.solve

import codes.lyndon.crypt.CipherUtils
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartFrame, ChartUtils, JFreeChart}
import org.jfree.data.category.DefaultCategoryDataset

import java.io.File
import scala.collection.mutable
import scala.io.Source

case class LetterFrequency(
    private val map: Map[Char, Int]
) {
  val totalCount: Int = map.values.sum

  def apply(c: Char): Int = map.getOrElse(c, 0)

  def ratio(c: Char): Double =
    map.getOrElse(c, 0).toDouble / totalCount.toDouble

  def ordered: Seq[(Char, Int, Double)] =
    map.toSeq
      .sortWith {
        case ((_, freq1), (_, freq2)) =>
          freq1 > freq2
      }
      .map {
        case (c, count) => (c, count, count.toDouble / totalCount.toDouble)
      }

  def letterOrder: Seq[(Char, Int, Double)] =
    map.toSeq
      .sortWith {
        case ((c1, _), (c2, _)) =>
          c1 < c2
      }
      .map {
        case (c, count) => (c, count, count.toDouble / totalCount.toDouble)
      }

  def asCsv(sep: String = ","): String = {
    val builder = new StringBuilder()
    builder.append(s"Char${sep}Count${sep}Ratio\n")
    ordered
      .map {
        case (c, count, ratio) =>
          s"$c$sep$count$sep$ratio\n"
      }
      .map(builder.append)
    builder.mkString
  }

  def makeChart(title: String = "Letter Frequency"): JFreeChart = {

    val dataset = new DefaultCategoryDataset()
    letterOrder.foreach {
      case (letter, count, ratio) =>
        dataset.addValue(ratio, "letter", letter)
    }

    ChartFactory.createBarChart(
      title,
      "Letter",
      "Frequency",
      dataset,
      PlotOrientation.VERTICAL,
      true,
      true,
      false
    )
  }

  def saveChart(file: File): Unit = {
    ChartUtils.saveChartAsPNG(file, makeChart(), 800, 600)
  }

  def displayChart(): Unit = {
    val frame = new ChartFrame("Frequency", makeChart())
    frame.setSize(800, 600)
    frame.pack()
    frame.setVisible(true)
  }

  override def toString: String = {
    val builder = new StringBuilder()
    builder.append(s"Total: $totalCount\n")
    builder.append("Char\tCount\tRatio\n")
    ordered
      .map {
        case (c, count, ratio) =>
          s"$c\t$count\t$ratio\n"
      }
      .map(builder.append)
    builder.mkString
  }
}

object LetterFrequency {

  def basedOn(
      file: File,
      preprocess: String => String = identity
  ): LetterFrequency = {
    val source = Source.fromFile(file)
    try {
      val counts = source.getLines().map(preprocess).map(countChars)
      val combined = counts.flatten.foldLeft(mutable.Map.empty[Char, Int]) {
        case (existing, (c, count)) =>
          val total = existing.get(c).map(_ + count).getOrElse(count)
          existing.put(c, total)
          existing
      }

      LetterFrequency(combined.toMap)
    } finally {
      source.close()
    }
  }

  def basedOn(string: String): LetterFrequency =
    LetterFrequency(countChars(string))

  private[this] def countChars(string: String): Map[Char, Int] =
    string.groupBy(identity).view.mapValues(_.length).toMap

  val english: Seq[(Char, Double)] = Seq(
    ('A', 0.082),
    ('B', 0.015),
    ('C', 0.028),
    ('D', 0.043),
    ('E', 0.13),
    ('F', 0.022),
    ('G', 0.02),
    ('H', 0.061),
    ('I', 0.07),
    ('J', 0.0015),
    ('K', 0.0077),
    ('L', 0.04),
    ('M', 0.024),
    ('N', 0.067),
    ('O', 0.075),
    ('P', 0.019),
    ('Q', 0.00095),
    ('R', 0.06),
    ('S', 0.063),
    ('T', 0.091),
    ('U', 0.028),
    ('V', 0.0098),
    ('W', 0.024),
    ('X', 0.0015),
    ('Y', 0.02),
    ('Z', 0.00074)
  )

  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      println("Missing file to process")
    }

    val file = new File(args(0))
    val freq = LetterFrequency.basedOn(
      file,
      CipherUtils.StringMethods(_).prepareForCipher()
    )
    println(freq)

    freq.displayChart()
//    val image = File.createTempFile("chart", ".png")
//    freq.saveChart(image)
//    print(image.getAbsolutePath)
  }
}
