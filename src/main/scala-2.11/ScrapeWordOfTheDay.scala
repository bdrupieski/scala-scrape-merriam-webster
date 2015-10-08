import java.io.PrintWriter

import org.joda.time.{DateTime, Days}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, Document}
import org.jsoup.select.Elements

import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions.collectionAsScalaIterable

object ScrapeWordOfTheDay {
  def main(args: Array[String]) {

    val startingDate = new DateTime(2014, 1, 1, 0, 0)
    val endingDate = startingDate.plusYears(1)

    val numberOfDays = Days.daysBetween(startingDate, endingDate).getDays
    val dates = for (f <- 0 until numberOfDays) yield startingDate.plusDays(f)

    val outLines = new ArrayBuffer[String]()

    for(d <- dates) {
      println(s"processing $d")
      outLines.append(getLineForDay(d))
      Thread.sleep(500)
    }

    val outWriter = new PrintWriter("2014wordsoftheday.txt")
    outLines.foreach(outWriter.write)
    outWriter.close()
  }

  def getLineForDay(date: DateTime): String = {
    val urlToGet = f"http://www.merriam-webster.com/word-of-the-day/${date.getYear}%d/${date.getMonthOfYear}%02d/${date.getDayOfMonth}%02d/"

    val doc: Document = Jsoup.connect(urlToGet).get()
    val wordContainer: Elements = doc.select(".wod_container")

    val word = wordContainer.select(".wod_headword").text
    val pronunciation = wordContainer.select(".wod_pron").text
    val partOfSpeech = wordContainer.select(".wod_pos").text
    val defs = getHtmlDefinitionsFromBlock(wordContainer.select(".d").head)
    val examples = wordContainer.select(".wod_example_sentences").html.replace("\n", "")

    s"$word ~ $word $pronunciation <br><br> $partOfSpeech <br><br> $defs <br><br><br> $examples ${sys.props("line.separator")}"
  }

  def getHtmlDefinitionsFromBlock(definitionElement: Element): String = {
    val singleDefBlock = definitionElement.select(".sense-block-one")
    if (!singleDefBlock.isEmpty) {
      singleDefBlock.text().replaceFirst(":", "")
    } else {
      val defBlocks = definitionElement.select(".sblk")
      val stringBuilder = new StringBuilder()
      for (block <- defBlocks) {
        val num: String = block.select(".snum").text
        val defsForBlock = block.select(".scnt").select(".ssens").map(x => s"$num${x.text}").reduce((x, y) => s"$x<br><br>$y")
        stringBuilder.append(defsForBlock + "<br><br>")
      }
      stringBuilder.toString()
    }
  }
}