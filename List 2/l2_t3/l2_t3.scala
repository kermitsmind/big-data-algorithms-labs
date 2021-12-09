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

    // making shingles out of words' substrings
    // def convertStringArrayIntoShinglesArray(givenArray: Array[String], k: Int): Array[String] = {
    //     val shinglesFrequencyMap: concurrent.Map[String, Int] = new concurrent.TrieMap()
    //     for (word <- givenArray){
    //         var shingles: Array[String] = generateStringShingles(word, k)
    //         for (shingle <- shingles){
    //             if(shinglesFrequencyMap.keys.toArray contains shingle){
    //                 var oldFrequency: Int = shinglesFrequencyMap.getOrElse(shingle, 0)
    //                 var newFrequency:Int = oldFrequency + 1
    //                 shinglesFrequencyMap.replace(shingle, newFrequency)
    //             }else{
    //                 shinglesFrequencyMap.putIfAbsent(shingle, 1)
    //             }                
    //         }
    //     }
    //     shinglesFrequencyMap.remove("")
    //     return shinglesFrequencyMap.keys.toArray
    // }

    //making shingles out of whole words
    def convertStringArrayIntoShinglesArray(givenArray: Array[String], k: Int): Array[String] = {
        val shinglesFrequencyMap: concurrent.Map[String, Int] = new concurrent.TrieMap()
        for (wordIndex <- 0 to givenArray.size){
            var maxShingleIndex: Int = min((givenArray.size - 1), (wordIndex + k - 1))
            var shingle: String = ""
            for (shingleIndex <- wordIndex to maxShingleIndex){
                if ((maxShingleIndex - wordIndex + 1) >= k){
                    shingle = shingle + " " + givenArray(shingleIndex)
                }
            }                
            if(shinglesFrequencyMap.keys.toArray contains shingle){
                var oldFrequency: Int = shinglesFrequencyMap.getOrElse(shingle, 0)
                var newFrequency:Int = oldFrequency + 1
                shinglesFrequencyMap.replace(shingle, newFrequency)
            }else{
                shinglesFrequencyMap.putIfAbsent(shingle, 1)
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

    def createSignatureMatrix(arrayOfDocumentNames: Array[String], n: Int, k: Int): Array[Array[Int]] = {
        // create union set of shingles common for all the books
        var commonShingles: Set[String] = generateBookShingles(book_name = arrayOfDocumentNames(0), k = k)
        var tempShingles: Set[String] = Set()
        for (book <- arrayOfDocumentNames){
            tempShingles = generateBookShingles(book_name = book, k = k)
            commonShingles = commonShingles.union(tempShingles)
        }
        
        // create initial signatue matrix
        var initialSignatureMatrix: Array[Array[Int]] = Array()
        for (book <- arrayOfDocumentNames){
            var columnSignature: Array[Int] = Array()
            var setShingles: Set[String] = generateBookShingles(book_name = book, k = k)
            for (shingle <- commonShingles){
                if (setShingles.contains(shingle)){
                    columnSignature = columnSignature :+ 1
                }else{
                    columnSignature = columnSignature :+ 0
                }
            }
            initialSignatureMatrix = initialSignatureMatrix :+ columnSignature
        }
    
        // initialize final signatue matrix
        var finalSignatureMatrix: Array[Array[Int]] = Array()
        val maxCommonSetValue: Int = commonShingles.size + 1
        for (book <- arrayOfDocumentNames){
            var columnSignature: Array[Int] = Array()
            var setShingles: Set[String] = generateBookShingles(book_name = book, k = k)
            for (_ <- 0 to (n - 1)){
                columnSignature = columnSignature :+ maxCommonSetValue
            }
            finalSignatureMatrix = finalSignatureMatrix :+ columnSignature
        }
    
        var hashTable: Array[Array[Int]] = generateRandomHashingTables(set = commonShingles, n = n)
        var signatureMatrixValue: Int = 0
        var hashTableValue: Int = 0
        // iterate over columns if signature matrix
        for (columnIndex <- 0 to (initialSignatureMatrix.size - 1)){
            for (rowIndex <- 0 to (initialSignatureMatrix(0).size - 1)){
                if (initialSignatureMatrix(columnIndex)(rowIndex) != 0){
                    for (i <- 0 to (n - 1)){
                        signatureMatrixValue = finalSignatureMatrix(columnIndex)(i)
                        hashTableValue = hashTable(i)(rowIndex)
                        finalSignatureMatrix(columnIndex)(i) = min(signatureMatrixValue, hashTableValue)
                    }
                }
            }
        }
        return finalSignatureMatrix
    }

    def computeJaccardSimilarityOnSignatures(signatureA: Array[Int], signatureB: Array[Int]): Float = {        
        // compute intersction and union
        val intersection: Set[Int] = signatureA.toSet.intersect(signatureB.toSet)
        val union: Set[Int] = signatureA.toSet.union(signatureB.toSet)
        
        // compute Jaccard similarity
        val jaccardSimilarity: Float = (intersection.size.toFloat) / (union.size.toFloat)
        
        return jaccardSimilarity
    }    

    def main(args: Array[String]) = {

        // book name array
        var booksArray: Array[String] = Array("books/gk_chesterton_1.txt", "books/gk_chesterton_2.txt",
                        "books/gk_chesterton_3.txt", "books/king.txt", "books/radziwill.txt", "books/shakespeare.txt")
        var testArray: Array[String] = Array("books/test.txt", "books/test2.txt", "books/test3.txt")

        // no. of hash functions
        var nRange: List[Int] = List(10, 100, 250, 500)

        // compute signature matrix
        var signatureMatrix: Array[Array[Int]] = Array()

        
        // calculate jaccard similarities
        val documentsArray: Array[String] = booksArray
        var jaccardSimilarity: Float = 0

        // testing new shingle creation
        // val sampleArray: Array[String] = Array("ala", "ma", "kocura", "hahaha")
        // val resArray: Array[String] = convertStringArrayIntoShinglesArray(sampleArray, 5)
        // for (shingle <- resArray){
        //     println("\n" + shingle)
        // }
        
        for (k <- 4 to 13){
            for (n <- nRange){
                // open file writer to save results
                val fileWriter = new FileWriter("l2_t3_results_newShingles.txt", true)
                
                signatureMatrix = createSignatureMatrix(arrayOfDocumentNames = documentsArray, n = n, k = k)
                print("\nk: " + k + " n: " + n)
                fileWriter.write("\nk: " + k + " n: " + n + "\n")
                fileWriter.write("\n")
                
                for (signatureIndexA <- 0 to 2){
                    for (signatureIndexB <- 3 to (signatureMatrix.size - 1)){
                        print("\n" + documentsArray(signatureIndexA) + " " + documentsArray(signatureIndexB))
                        jaccardSimilarity = computeJaccardSimilarityOnSignatures(signatureMatrix(signatureIndexA), signatureMatrix(signatureIndexB))
                        print(" = " + jaccardSimilarity.toString)
                        fileWriter.write("jacc sim of: " + documentsArray(signatureIndexA) + " and " + documentsArray(signatureIndexB) + " = " + jaccardSimilarity.toString)
                        fileWriter.write("\n")
                    }
                }
                
                // close file writer
                fileWriter.close()
            }
        }
    }
}