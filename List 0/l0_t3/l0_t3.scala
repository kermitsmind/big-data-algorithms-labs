// Resources:
    // https://stackoverflow.com/questions/5055349/how-to-take-input-from-a-user-in-scala/42968214
    // https://www.tutorialspoint.com/scala/scala_classes_objects.htm
    // https://alvinalexander.com/scala/scala-break-continue-examples-breakable/
    // https://github.com/scala/bug/issues/3839
    // https://data-flair.training/blogs/scala-string/

import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._
import util.control.Breaks._

class WordAnalyzer(){
    
    def getUserInput(): Array[String] = {
        println("User input: ")
        val userInputGet = scala.io.StdIn.readLine()
        val userInput = userInputGet.concat(" ")
        var importedWords : Array[String] = Array()
        var word: String = String()
        for (char <- userInput){
            breakable{
                if (char.isWhitespace){
                    // word.replaceAll("""[\p{Punct}]""", "")
                    println("----" + word)
                    importedWords = importedWords :+ word.toLowerCase()
                    word = ""
                    break
                } else{
                    var letter: String = char.toString()
                    word = word.concat(letter)
                }

            }
        }
        return importedWords
    }

    def importData(file_name: String): Array[String] = {
        var importedWords : Array[String] = Array()
        for (line <- Source.fromFile(file_name).getLines) {
            var words = line.split(" ")
            for (word <- words) {
                var newWord = word.replaceAll("""[\p{Punct}]""", "")
                importedWords = importedWords :+ newWord.toLowerCase()
            }
        }
        return importedWords
    }
    
    def removeElements(initialArray: Array[String], toBeRemovedArray: Array[String]): Array[String] = {
        var filteredArray : Array[String] = initialArray
        for (elementToBeRemoved <- toBeRemovedArray) {
            filteredArray = filteredArray.filterNot(element => element == elementToBeRemoved)
        }
        return filteredArray
    }
        
    def countWordsFrequency(givenArray: Array[String]): Map[String, Int] = {
        val wordsFrequencyMap: concurrent.Map[String, Int] = new concurrent.TrieMap()
        for (word <- givenArray){
            if(wordsFrequencyMap.keys.toArray contains word){
                var oldFrequency: Int = wordsFrequencyMap.getOrElse(word, 0)
                var newFrequency:Int = oldFrequency + 1
                wordsFrequencyMap.replace(word, newFrequency)
            }else{
                wordsFrequencyMap.putIfAbsent(word, 1)
            }
        }
        wordsFrequencyMap.remove("")
        return wordsFrequencyMap.toMap
    }

    def sortWordsMapDescending(givenMap: Map[String, Int]): Map[String, Int] = {
        val resultSortedMap: Map[String, Int] = ListMap(givenMap.toSeq.sortWith(_._2 > _._2):_*)
        return resultSortedMap
    }

    def sortWordsMapAscending(givenMap: Map[String, Int]): Map[String, Int] = {
    val resultSortedMap: Map[String, Int] = ListMap(givenMap.toSeq.sortWith(_._2 < _._2):_*)
    return resultSortedMap
    }


}


object MainObject {
    
    def main(args:Array[String]) = {

        val wordAnalyzerObject = new WordAnalyzer()
        var u = wordAnalyzerObject.getUserInput()
        for (w <- u){
            println(w)
        }
        // val bookFileName = "l0_t2_text.txt"
        // var bookWords = importData(bookFileName)
        
        // val stopWordsFileName = "stop_words_english.txt"
        // var stopWords = importData(stopWordsFileName)

        // var filteredWords = removeElements(bookWords, stopWords)
        

        // var arrayTest : Array[String] = Array("a", "b", "b", "c")
        // val resultMap: Map[String, Int] = countWordsFrequency(filteredWords)

        // val resultSortedMap: Map[String, Int] = ListMap(resultMap.toSeq.sortWith(_._2 > _._2):_*)

        // val resultSortedMapFirstN: Map[String, Int] = resultSortedMap.take(100)
        // print(resultSortedMapFirstN)
        
        // val resultCsvFileName: String = "resultFrequencyWords.csv"
        // val fw = new FileWriter(resultCsvFileName, true)
        // for (key <- resultSortedMapFirstN.keys){
        //     //fw.write(key + ", " + resultSortedMapFirstN.getOrElse(key, "-")) 
        //     //format fot word cloud generator
        //     fw.write("\"" + resultSortedMapFirstN.getOrElse(key, "-") + "\"" + ";" + "\"" + key + "\"") 
        //     fw.write("\n")
        // }
        // fw.close()

    }
}