import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._

object MainObject{

    def importData(file_name: String): Array[String] = {
        var importedWords : Array[String] = Array()
        var emptyArray : Array[String] = Array()
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
        return importedWords
    }

    def removeElements(initialArray: Array[String], toBeRemovedArray: Array[String]): Array[String] = {
        var filteredArray : Array[String] = initialArray
        for (elementToBeRemoved <- toBeRemovedArray) {
            filteredArray = filteredArray.filterNot(element => element == elementToBeRemoved)
        }
        return filteredArray
    }

    def generateStringShingles(string: String, k: Int): Array[String] = {
        var output: Array[String] = Array()
        val length: Int = string.length()
        val iterations: Int = length - k
        var substring: String = ""
        for (index <- 0 to iterations){
            substring = string.substring(index, index + k)
            output = output :+ substring
        }
        return output
    }

    def convertStringArrayIntoShinglesArray(givenArray: Array[String], k: Int): Array[String] = {
        val shinglesFrequencyMap: concurrent.Map[String, Int] = new concurrent.TrieMap()
        for (word <- givenArray){
            var shingles: Array[String] = generateStringShingles(word, k)
            for (shingle <- shingles){
                if(shinglesFrequencyMap.keys.toArray contains shingle){
                    var oldFrequency: Int = shinglesFrequencyMap.getOrElse(shingle, 0)
                    var newFrequency:Int = oldFrequency + 1
                    shinglesFrequencyMap.replace(shingle, newFrequency)
                }else{
                    shinglesFrequencyMap.putIfAbsent(shingle, 1)
                }                
            }
        }
        shinglesFrequencyMap.remove("")
        return shinglesFrequencyMap.keys.toArray
    }

    def main(args: Array[String]) = {
        // loading stopwords
        val stopWordsFileName: String = "stop_words_english.txt"
        val stopWordsStringArray: Array[String] = importData(file_name = stopWordsFileName)
        
        // loading a book
        var bookFileName: String = "books/gk_chesterton_1.txt"
        var initialStringArray: Array[String] = importData(file_name = bookFileName)
        var filteredStringArray: Array[String] = removeElements(initialArray = initialStringArray, toBeRemovedArray = stopWordsStringArray)
        
        // creating k-shigles out of the book words (shingles does not repeat)
        var k: Int = 4
        var shinglesArray: Array[String] = convertStringArrayIntoShinglesArray(givenArray = filteredStringArray, k = k)
        
    }
}