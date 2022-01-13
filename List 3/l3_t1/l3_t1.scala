// references: 
    // https://alvinalexander.com/scala/scala-how-to-download-url-contents-to-string-file/
    // https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
    // https://stackoverflow.com/questions/2013124/regex-matching-up-to-the-first-occurrence-of-a-character
    // https://stackoverflow.com/questions/2078915/a-regular-expression-to-exclude-a-word-string
    // https://www.geeksforgeeks.org/scala-string-replace-method-with-example/
    // https://alvinalexander.com/scala/how-to-generate-random-numbers-characters-sequences-in-scala/
    // https://www.tutorialspoint.com/scala/scala_arrays.htm
    // https://www.geeksforgeeks.org/for-loop-in-scala/
    // https://alvinalexander.com/scala/how-to-open-read-text-files-in-scala-cookbook-examples/


import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._
import scala.math._
import Array._
import scala.util.matching.Regex

object MainObject{

    ////////////////// CRAWL MODE 
    // take name, string array and saves to file with given name
    def writeFile(fileName: String, stringArray: Array[String]): Unit = {
        val fileWriter = new FileWriter(fileName, true)
        for (line <- stringArray){
            fileWriter.write(line)
            fileWriter.write("\n")
        }
        fileWriter.close()
    }

    // take url, obtain wiki/Topic hrefs and save them to file, returns one link out of the found ones (randomly)
    def crawlPageFromWeb(url: String): String = {
        val pageContent = Source.fromURL(url).mkString
        val pageName = "wiki\\/[^\"]*".r.findAllIn(url).next.replace('/', '_')
        // below regex mathes many url's however on wikipedia wikipedia's subpages are linked via 'wiki/Topic' thus below regex won't work as desired
        // val generalHrefPattern = "(https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})".r
        val wikiHrefPattern = "wiki\\/(?!.*%|Category|Help|Wikipedia|Special|" +
        "Talk|Privacy_policy|Cookie_statement|Terms_of_Use|Portal|Main_Page|File|Template)[^\"]*"
        val hrefs = wikiHrefPattern.r.findAllIn(pageContent).toSet.toArray
        writeFile(fileName = ("webPages/" + pageName), stringArray = hrefs)
        val randomNumberGenerator = scala.util.Random
        val randomNumber = randomNumberGenerator.nextInt(hrefs.length)
        return ("https://en.wikipedia.org/" + hrefs(randomNumber))
    }

    // take initial web page url and number n, crawl n pages and save them to files
    def crawlGivenNumberOfPages(url: String, n: Int): Unit = {
        val crawledPagesListName: String = "crawledPagesList"
        var crawledPagesListArray: Array[String] = Array()
        crawledPagesListArray = crawledPagesListArray.appended(url)
        var nextUrlToVisit: String = crawlPageFromWeb(url = url)
        var remainingPagesToVisit: Int = n -1
        for (i <- 1 to remainingPagesToVisit){
            crawledPagesListArray = crawledPagesListArray.appended(nextUrlToVisit)
            nextUrlToVisit = crawlPageFromWeb(url = nextUrlToVisit)
        }
        writeFile(fileName = crawledPagesListName, stringArray = crawledPagesListArray)
    }

    ////////////////// PAGE_RANK MODE
    // take name, read file and return string array with file content
    def readFile(fileName: String): Array[String] = {
        var readArray: Array[String] = Source.fromFile(fileName).getLines.toArray
        return readArray
    }
    //def createTransitionMatrixOfCrawledPages



    // main function
    def main(args: Array[String]): Unit = {
        val html = "https://en.wikipedia.org/wiki/Web_crawler"
        // crawlGivenNumberOfPages(url = html, n = 10)
        val rTest: Array[String] = readFile(fileName = "crawledPagesList")
        for (line <- rTest){
            println(line)
        }
        
    }   

}