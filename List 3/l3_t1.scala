// references: 
    // https://alvinalexander.com/scala/scala-how-to-download-url-contents-to-string-file/
    // https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
    // https://stackoverflow.com/questions/2013124/regex-matching-up-to-the-first-occurrence-of-a-character
    // https://stackoverflow.com/questions/2078915/a-regular-expression-to-exclude-a-word-string


import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._
import scala.math._
import Array._
import scala.util.matching.Regex

object MainObject{

    // takes url and return string array of wiki/Topic hrefs
    def crawlPageFromWeb(url: String): Array[String] = {
        val pageContent = Source.fromURL("https://en.wikipedia.org/wiki/Web_crawler").mkString
        // below regex mathes many url's however on wikipedia wikipedia's subpages are linked via 'wiki/Topic' thus below regex won't work as desired
        // val generalHrefPattern = "(https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})".r
        val wikiHrefPattern = "wiki\\/(?!.*%|Category|Help|Wikipedia|Special|" +
          "Talk|Privacy_policy|Cookie_statement|Terms_of_Use|Portal|Main_Page|File|Template)[^\"]*"
        val hrefs = wikiHrefPattern.r.findAllIn(pageContent).toSet.toArray
        return hrefs
    }



    // main function
    def main(args: Array[String]): Unit = {
        val html = "https://en.wikipedia.org/wiki/Web_crawler"
        val testArray = crawlPageFromWeb(html)
        for (a <- testArray){println(a)}
    }   

}