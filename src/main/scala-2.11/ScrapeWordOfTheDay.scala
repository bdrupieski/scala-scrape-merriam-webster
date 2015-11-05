import java.io.PrintWriter

import org.joda.time.{DateTime, Days}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, Document}
import org.jsoup.select.Elements

import scala.collection.mutable.ArrayBuffer

import scala.collection.JavaConversions.collectionAsScalaIterable

object ScrapeWordOfTheDay {
  def main(args: Array[String]) {

    val year: Int = 2013
    val startingDate = new DateTime(year, 1, 1, 0, 0)
    val endingDate = startingDate.plusYears(1)

    val numberOfDays = Days.daysBetween(startingDate, endingDate).getDays
    val dates = for (f <- 0 until numberOfDays) yield startingDate.plusDays(f)

    val outLines = new ArrayBuffer[String]()

    for (d <- dates) {
      println(s"processing $d")
      outLines.append(getLineForDay(d))
    }

    val outWriter = new PrintWriter(f"${year}wordsoftheday.txt")
    outLines.foreach(outWriter.write)
    outWriter.close()
  }

  def getDocumentFromUrl(url: String): Document = {
    Thread.sleep(1000)
    Jsoup.connect(url).get()
  }

  def getLineForDay(date: DateTime): String = {
    val wotdUrl = f"http://www.merriam-webster.com/word-of-the-day/${date.getYear}%d/${date.getMonthOfYear}%02d/${date.getDayOfMonth}%02d/"

    val doc: Document = getDocumentFromUrl(wotdUrl)
    val wordContainer: Elements = doc.select(".wod_container")

    val word = wordContainer.select(".wod_headword").text
    val pronunciation = wordContainer.select(".wod_pron").text
    val partOfSpeech = wordContainer.select(".wod_pos").text
    val defs = getHtmlDefinitionsFromBlock(wordContainer.select(".d").head)
    val examples = wordContainer.select(".wod_example_sentences").html.replace("\n", "")
    val (etymology, synonyms) = getEtymologyAndSynonyms(word)

    s"$word~" +
      s"$word $pronunciation <br><br>" +
      s"$partOfSpeech <br><br>" +
      s"$defs <br><br><br>" +
      s"$examples <br><br><br>" +
      s"$etymology <br><br><br>" +
      s"$synonyms <br><br><br>" +
      s"${sys.props("line.separator")}"
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

  // Etymology and synonyms are not found on the WOTD page.
  // They must be retrieved from the definition page for that word.
  def getEtymologyAndSynonyms(word: String): (String, String) = {

    val wordDefUrl = f"http://www.merriam-webster.com/dictionary/$word"
    val doc: Document = getDocumentFromUrl(wordDefUrl)

    val etymologyElements = doc.select(".etymology")
    etymologyElements.select("a").unwrap()

    val synonymsElements = doc.select(".synonyms-reference")
    synonymsElements.select("a").unwrap()
    synonymsElements.select(".accordion-heading").remove()

    val etymology: String = etymologyElements.html.replace("\n", "")
    val synonyms: String = synonymsElements.html.replace("\n", "")

    (etymology, synonyms)
  }
}