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
    var stopWords = importFreshData(stopWordsFileName)

    var workingWordsFrequencyMap: Map[String, Int] = Map()
    var workingArrayOfWords : Array[String] = Array()

    var workingDocumentsMap: Map[String, Array[String]] = Map()
    var workingDocuments_TF_IDF_fMap: Map[String, Map[String, Float]] = Map()
    var loadedArrayDocuments : Array[String] = Array()

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
        var resultWords : Array[String] = workingArrayOfWords ++ importedWords
        workingArrayOfWords = resultWords
        // return importedWords
    }

    def importFreshData(file_name: String): Array[String] = {
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
        workingArrayOfWords = importedWords
        loadedArrayDocuments = emptyArray
        loadedArrayDocuments = loadedArrayDocuments :+ file_name
        return importedWords
    }

    def importIntoDocumentsMap(file_name: String): Unit = {
        var importedWords : Array[String] = importFreshData(file_name=file_name)
        workingDocumentsMap += (file_name -> importedWords)
    }

    def printLoadedMapDocuments(): Unit = {
        for (key <- workingDocumentsMap.keys){
            print(" -- " + key)
        }
    }

    def printLoadedArrayDocuments(): Unit = {
        for (document <- loadedArrayDocuments){
            print(" -- " + document)
        }
    }

    def addSelectedDocumentIntoArray(): Unit = {
        println("Document name: ")
        val documentName = scala.io.StdIn.readLine()
        var resultWords : Array[String] = workingArrayOfWords ++ workingDocumentsMap(documentName)
        workingArrayOfWords = resultWords
        loadedArrayDocuments = loadedArrayDocuments :+ documentName
    }

    def importAppendData(file_name: String): Array[String] = {
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
        var resultImportedWords = workingArrayOfWords ++ importedWords
        loadedArrayDocuments = loadedArrayDocuments :+ file_name
        return resultImportedWords
    }
    
    def removeElements(initialArray: Array[String] = workingArrayOfWords, toBeRemovedArray: Array[String] = stopWords): Unit = {
        var filteredArray : Array[String] = initialArray
        for (elementToBeRemoved <- toBeRemovedArray) {
            filteredArray = filteredArray.filterNot(element => element == elementToBeRemoved)
        }
        workingArrayOfWords = filteredArray
        // return filteredArray
    }

    def refreshArray(givenArray: Array[String] = workingArrayOfWords): Unit = {
        var newArrayOfWords : Array[String] = Array()
        workingArrayOfWords = newArrayOfWords
        loadedArrayDocuments = newArrayOfWords
    }

    def printArrayFirstN(givenArray: Array[String] = workingArrayOfWords, n: Int): Unit = {
        val resultTakenArray: Array[String] = workingArrayOfWords
        val resultTakenList: List[String] = resultTakenArray.toList
        for (word <- resultTakenList.take(n)){
            print(word + " ")
        }
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

    def refreshMap(givenMap: Map[String, Int] = workingWordsFrequencyMap): Unit = {
        var newWordsFrequencyMap: Map[String, Int] = Map()
        workingWordsFrequencyMap = newWordsFrequencyMap
    }

    def compute_TF_IDF(documentID: String, words: Array[String]): Unit{
        
    }
}


object MainObject {
    
    def main(args:Array[String]) = {

        val wordAnalyzerObject = new WordAnalyzer()
        // var workingArrayOfWords : Array[String] = Array()
        var loopCondition: Boolean = true
        breakable{
            while(loopCondition){
                print("\n|----------------")
                print("\n|Loaded into map:\n|")
                wordAnalyzerObject.printLoadedMapDocuments()
                print("\n|Loaded into array:\n|")
                wordAnalyzerObject.printLoadedArrayDocuments()
                print("\n\nLOOP action: ")
                var loopUserInput = scala.io.StdIn.readLine()
                loopUserInput match {
                    case "user input" => {
                        wordAnalyzerObject.getUserInput()
                    }

                    case "import into map" => {
                        // add document into map for further actions
                        print("File name: ")
                        var fileName = scala.io.StdIn.readLine()
                        wordAnalyzerObject.importIntoDocumentsMap(fileName)
                    }

                    case "import from map into array" => {
                        // add document from map into array for further actions
                        wordAnalyzerObject.addSelectedDocumentIntoArray()
                    }

                    case "import fresh data" => {
                        // create new empty array and import data
                        print("File name: ")
                        var fileName = scala.io.StdIn.readLine()
                        wordAnalyzerObject.importFreshData(fileName)
                    }

                    case "import append data" => {
                        // import data and append to an existing array
                        print("File name: ")
                        var fileName = scala.io.StdIn.readLine()
                        wordAnalyzerObject.importAppendData(fileName)
                    }

                    case "array remove stopwords" => {
                        wordAnalyzerObject.removeElements()
                    }

                    case "refresh array" => {
                        wordAnalyzerObject.refreshArray()
                    }                    

                    case "print array" => {
                        println("Number of elements: ")
                        var numberOfelements = scala.io.StdIn.readLine()
                        wordAnalyzerObject.printArrayFirstN(n = numberOfelements.toInt)
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

                    case "sort map ascending" => {
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

                    case "refresh map" => {
                        wordAnalyzerObject.refreshMap()
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