import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._
import scala.math._
import scala.util.Random
import Array._

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

    def generateBookShingles(book_name: String, k: Int): Set[String] = {
        // loading stopwords
        val stopWordsFileName: String = "stop_words_english.txt"
        val stopWordsStringArray: Array[String] = importData(file_name = stopWordsFileName)
        
        // loading a book
        var initialStringArray: Array[String] = importData(file_name = book_name)
        var filteredStringArray: Array[String] = removeElements(initialArray = initialStringArray, toBeRemovedArray = stopWordsStringArray)
        
        // creating k-shigles out of the book words (shingles does not repeat)
        var shinglesArray: Array[String] = convertStringArrayIntoShinglesArray(givenArray = filteredStringArray, k = k)

        // return shingles array
        return shinglesArray.toSet
    }

    def computeJaccardSimilarity(bookNameA: String, bookNameB: String, k: Int): Float = {
        // generate shingle arrays
        var shinglesA = generateBookShingles(book_name = bookNameA, k = k)
        var shinglesB = generateBookShingles(book_name = bookNameB, k = k)
        
        // compute intersction and union
        val intersection: Set[String] = shinglesA.intersect(shinglesB)
        val union: Set[String] = shinglesA.union(shinglesB)
        
        // compute Jaccard similarity
        val jaccardSimilarity: Float = (intersection.size.toFloat) / (union.size.toFloat)
        
        return jaccardSimilarity
    }

    def generateRandomHashingTables(set: Set[String], n: Int): Array[Array[Int]] = {
        val setSize: Int = set.size
        var arrayInner: Array[Int] = range(0, setSize)
        var arrayInnerShuffled: Array[Int] = Array()
        var arrayOuter: Array[Array[Int]] = Array()

        for (_ <- 1 to n){
            arrayInnerShuffled = Random.shuffle(arrayInner.toList).toArray
            arrayOuter = arrayOuter :+ arrayInnerShuffled
        }

        return arrayOuter
    }



    def main(args: Array[String]) = {

        // book name array
        var mixedBooksArray1: Array[String] = Array("gk_chesterton_1.txt", "gk_chesterton_2.txt",
                        "gk_chesterton_3.txt")
        var mixedBooksArray2: Array[String] = Array("king.txt", "radziwill.txt", 
                        "shakespeare.txt")
        var testArray: Array[String] = Array("books/test.txt", "books/test2.txt", 
                        "books/test3.txt")

        // shingle length
        var k: Int = 4
        
        // create union set of shingles common for all the books
        var commonShingles: Set[String] = generateBookShingles(book_name = testArray(0), k = k)
        var tempShingles: Set[String] = Set()
        for (book <- testArray){
            tempShingles = generateBookShingles(book_name = book, k = k)
            commonShingles = commonShingles.union(tempShingles)

        }
        
        // create initial signatue matrix
        var initialSignatureMatrix: Array[Array[Int]] = Array()
        for (book <- testArray){
            var columnSignature: Array[Int] = Array()
            var setShingles: Set[String] = generateBookShingles(book_name = book, k = k)
            for (shingle <- commonShingles){
                if (setShingles.contains(shingle)){
                    columnSignature = columnSignature :+ 1
                }else{
                    columnSignature = columnSignature :+ 1
                }
            }
            initialSignatureMatrix = initialSignatureMatrix :+ columnSignature
        }

        

    }
}