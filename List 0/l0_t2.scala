// Resources:
// https://alvinalexander.com/scala/how-to-open-read-text-files-in-scala-cookbook-examples/
// https://stackoverflow.com/questions/14469958/how-to-split-sentence-into-words-separated-by-multiple-spaces
// https://stackoverflow.com/questions/30074109/removing-punctuation-marks-form-text-in-scala-spark
// http://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-filter-filternot-function/
// https://www.programcreek.com/scala/scala.collection.concurrent
// https://www.scala-lang.org/api/2.12.9/scala/collection/concurrent/Map.html
// https://alvinalexander.com/scala/how-to-sort-map-in-scala-key-value-sortby-sortwith/
// https://stackoverflow.com/questions/18018676/select-first-n-elements-from-map-in-scala
// https://www.geeksforgeeks.org/scala-string-tolowercase-method-with-example/

import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap

object MainObject {
    
    def main(args:Array[String]) = {

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
        
        val bookFileName = "l0_t2_text.txt"
        var bookWords = importData(bookFileName)
        
        val stopWordsFileName = "stop_words_english.txt"
        var stopWords = importData(stopWordsFileName)

        var filteredWords = removeElements(bookWords, stopWords)
        

        // var arrayTest : Array[String] = Array("a", "b", "b", "c")
        val resultMap: Map[String, Int] = countWordsFrequency(filteredWords)

        val resultSortedMap: Map[String, Int] = ListMap(resultMap.toSeq.sortWith(_._2 > _._2):_*)

        // println("keys: " + resultSortedMap.keys)
        // println("vals: " + resultSortedMap.values)

        val resultSortedMapFirstN: Map[String, Int] = resultSortedMap.take(10)
        print(resultSortedMapFirstN)
        
        // code below shows how to use concurrent maps
        // import scala.collection.concurrent - this needs to be imported
        // val results: concurrent.Map[String, Int] = new concurrent.TrieMap()
        // results.putIfAbsent("a", 1)
        // results.putIfAbsent("a", 2)
        // results.putIfAbsent("b", 2)
        // val results: concurrent.Map[String, Int].asScala = concurrent.Map[String, Int]()
        // val m1 = mapTest + ("a" -> 1)
        // mapTest = m1
        // println("keys: " + results.keys)
        // println("values: " + results.values)
        // println("get: " + results.get("a"))
        // results.replace("a", 7)
        // println("keys: " + results.keys)
        // println("values: " + results.values)

        // val test: String = "xx"
        // if(results contains(test)){
        //     println("CONTAINS")
        // }else{
        //     println("DOES NOT CONTAIN")
        // }



    }
}