// Resources:
    // https://stackoverflow.com/questions/5055349/how-to-take-input-from-a-user-in-scala/42968214
    // https://www.tutorialspoint.com/scala/scala_classes_objects.htm
    // https://alvinalexander.com/scala/scala-break-continue-examples-breakable/
    // https://github.com/scala/bug/issues/3839
    // https://data-flair.training/blogs/scala-string/
    // https://alvinalexander.com/scala/break-continue-for-while-loops-in-scala-examples-how-to/
    // https://www.tutorialspoint.com/scala/scala_while_loop.htm
    // https://stackoverflow.com/questions/3639150/how-to-use-switch-case-simple-pattern-matching-in-scala
    // https://stackoverflow.com/questions/20148556/scala-case-match-default-value
    // https://docs.scala-lang.org/overviews/scala-book/try-catch-finally.html

import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._
import util.control.Breaks._

class WordAnalyzer(){
    
    val stopWordsFileName = "stop_words_english.txt"
    var stopWords = importData(stopWordsFileName)

    var workingWordsFrequencyMap: Map[String, Int] = Map()
    var workingArrayOfWords : Array[String] = Array()

    def getUserInput(): Unit = {
        println("User input: ")
        val userInputGet = scala.io.StdIn.readLine()
        val userInput = userInputGet.concat(" ")
        var importedWords : Array[String] = Array()
        var word: String = String()
        for (char <- userInput){
            breakable{
                if (char.isWhitespace){
                    word.replaceAll("""[\p{Punct}]""", "")
                    importedWords = importedWords :+ word.toLowerCase()
                    word = ""
                    break
                } else{
                    var letter: String = char.toString()
                    word = word.concat(letter)
                }

            }
        }
        workingArrayOfWords = importedWords
        // return importedWords
    }

    def importData(file_name: String): Array[String] = {
        var importedWords : Array[String] = Array()
        try {
            for (line <- Source.fromFile(file_name).getLines) {
                var words = line.split(" ")
                for (word <- words) {
                    var newWord = word.replaceAll("""[\p{Punct}]""", "")
                    importedWords = importedWords :+ newWord.toLowerCase()
                }
            }
        }catch{
            case e: FileNotFoundException => println("Such file does not exist")
        }
        workingArrayOfWords = importedWords
        return importedWords
    }
    
    def removeElements(initialArray: Array[String] = workingArrayOfWords, toBeRemovedArray: Array[String] = stopWords): Unit = {
        var filteredArray : Array[String] = initialArray
        for (elementToBeRemoved <- toBeRemovedArray) {
            filteredArray = filteredArray.filterNot(element => element == elementToBeRemoved)
        }
        workingArrayOfWords = filteredArray
        // return filteredArray
    }
        
    def countWordsFrequency(givenArray: Array[String] = workingArrayOfWords): Map[String, Int] = {
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
        workingWordsFrequencyMap = wordsFrequencyMap.toMap
        // return wordsFrequencyMap.toMap
        return workingWordsFrequencyMap
    }

    def sortWordsMapDescending(givenMap: Map[String, Int] = workingWordsFrequencyMap): Map[String, Int] = {
        val resultSortedMap: Map[String, Int] = ListMap(givenMap.toSeq.sortWith(_._2 > _._2):_*)
        workingWordsFrequencyMap = resultSortedMap
        return resultSortedMap
    }

    def sortWordsMapAscending(givenMap: Map[String, Int] = workingWordsFrequencyMap): Map[String, Int] = {
        val resultSortedMap: Map[String, Int] = ListMap(givenMap.toSeq.sortWith(_._2 < _._2):_*)
        workingWordsFrequencyMap = resultSortedMap
        return resultSortedMap
    }
    
    def printMapFirstN(givenMap: Map[String, Int] = workingWordsFrequencyMap, n: Int): Unit = {
        val resultTakenMap: Map[String, Int] = workingWordsFrequencyMap
        println(resultTakenMap.take(n))
    }
    
    def saveMapFirstN(givenMap: Map[String, Int] = workingWordsFrequencyMap, n: Int, resultCsvFileName: String): Unit = {
        val workingTakenMap: Map[String, Int] = workingWordsFrequencyMap
        val resultTakenMap = workingTakenMap.take(n)
        
        val fw = new FileWriter(resultCsvFileName, true)
        for (key <- resultTakenMap.keys){
            fw.write("\"" + resultTakenMap.getOrElse(key, "-") + "\"" + ";" + "\"" + key + "\"") 
            fw.write("\n")
        }
        fw.close()
    }
}


object MainObject {
    
    def main(args:Array[String]) = {

        val wordAnalyzerObject = new WordAnalyzer()
        // var workingArrayOfWords : Array[String] = Array()
        var loopCondition: Boolean = true
        breakable{
            while(loopCondition){
                print("\nLOOP action: ")
                var loopUserInput = scala.io.StdIn.readLine()
                loopUserInput match {
                    case "user input" => {
                        wordAnalyzerObject.getUserInput()
                    }

                    case "load file" => {
                        print("File name: ")
                        var fileName = scala.io.StdIn.readLine()
                        wordAnalyzerObject.importData(fileName)
                    }

                    case "array remove stopwords" => {
                        wordAnalyzerObject.removeElements()
                    }

                    case "count words frequency" => {
                        val resultMap: Map[String, Int] = wordAnalyzerObject.countWordsFrequency()
                        val resultSortedMapFirstN: Map[String, Int] = resultMap.take(10)
                        println(resultSortedMapFirstN)
                    }

                    case "sort map descending" => {
                        var sortedWordsMapDescending: Map[String, Int] = wordAnalyzerObject.sortWordsMapDescending()
                        val resultSortedMapFirstN: Map[String, Int] = sortedWordsMapDescending.take(10)
                        println(resultSortedMapFirstN)
                    }

                    case "sort map asscending" => {
                        var sortedWordsMapAscending: Map[String, Int] = wordAnalyzerObject.sortWordsMapAscending()
                        val resultSortedMapFirstN: Map[String, Int] = sortedWordsMapAscending.take(10)
                        println(resultSortedMapFirstN)
                    }

                    case "print map" => {
                        println("Number of elements: ")
                        var numberOfelements = scala.io.StdIn.readLine()
                        wordAnalyzerObject.printMapFirstN(n = numberOfelements.toInt)
                    }

                    case "save map" => {
                        println("Number of elements: ")
                        var numberOfelements = scala.io.StdIn.readLine()
                        println("CSV file name: ")
                        var fileName = scala.io.StdIn.readLine()
                        wordAnalyzerObject.saveMapFirstN(n = numberOfelements.toInt, resultCsvFileName = fileName)
                    }

                    case "break" => {
                        loopCondition = false
                    }
                    case _ => {
                        println("not recognized action, try again")
                    }
                }
            }
        }        
    }
}