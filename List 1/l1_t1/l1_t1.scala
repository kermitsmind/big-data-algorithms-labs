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
import scala.math._

class WordAnalyzer(){
    
    val stopWordsFileName = "stop_words_english.txt"
    var stopWords = importFreshData(stopWordsFileName)

    var workingWordsFrequencyMap: Map[String, Int] = Map()
    var workingArrayOfWords : Array[String] = Array()

    var workingDocumentsMap: Map[String, Array[String]] = Map()
    var workingDocuments_TF_IDF_Map: concurrent.Map[String, Map[String, Double]] = new concurrent.TrieMap()
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
        // workingArrayOfWords = importedWords
        //loadedArrayDocuments = emptyArray
        //loadedArrayDocuments = loadedArrayDocuments :+ file_name
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

    def addSelectedDocumentIntoArray(file_name: String): Unit = {
        var resultWords : Array[String] = workingArrayOfWords ++ workingDocumentsMap(file_name)
        workingArrayOfWords = resultWords
        loadedArrayDocuments = loadedArrayDocuments :+ file_name
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
    
    def removeElements(initialArray: Array[String] = workingArrayOfWords, toBeRemovedArray: Array[String] = stopWords): Array[String] = {
        var filteredArray : Array[String] = initialArray
        for (elementToBeRemoved <- toBeRemovedArray) {
            filteredArray = filteredArray.filterNot(element => element == elementToBeRemoved)
        }
        workingArrayOfWords = filteredArray
        return filteredArray
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

    def computeDocument_TF_IDF(documentID: String): Map[String, Double] = {
        var N: Int = loadedArrayDocuments.size // takes into account only loaded documents
        var n: Int = 0
        var TF: Double = 0
        var IDF: Double = 0
        var TF_IDF: Double = 0
        var log2 = (x: Double) => log10(x)/log10(2.0)
        var document_TF_IDF_Map: Map[String, Float] = Map()
        var wordsArray : Array[String] = workingDocumentsMap(documentID)
        var filteredWordsArray = removeElements(initialArray = wordsArray)
        var wordsFrequencyMap: Map[String, Int] = countWordsFrequency(givenArray = filteredWordsArray)
        var sortedWordsFrequencyMap: Map[String, Int] = sortWordsMapDescending(givenMap = wordsFrequencyMap)
        var tempWordsFrequencyMap_TF_IDF: concurrent.Map[String, Double] = new concurrent.TrieMap()
        var sortedWordsFrequencyMap_TF_IDF: Map[String, Double] = Map()
        var f_ij: Int = 0
        var f_ik: Int = sortedWordsFrequencyMap.head._2

        for (word <- sortedWordsFrequencyMap.keys){
            n = 0
            for (document <- workingDocumentsMap.keys){
                if (workingDocumentsMap(document) contains word){
                    n = n + 1
                }
            }
            IDF = log2(N/n.toDouble)
            f_ij = wordsFrequencyMap(word)
            TF = (f_ij.toDouble)/(f_ik.toDouble)
            TF_IDF = TF*IDF
            tempWordsFrequencyMap_TF_IDF.putIfAbsent(word, TF_IDF)
        }

        sortedWordsFrequencyMap_TF_IDF = ListMap(tempWordsFrequencyMap_TF_IDF.toSeq.sortWith(_._2 > _._2):_*)
        workingDocuments_TF_IDF_Map.putIfAbsent(documentID, sortedWordsFrequencyMap_TF_IDF)
        
        var TF_IDF_file: String = "TF-IDF_of__" + documentID + "__out_of_"
        for (document <- loadedArrayDocuments){
            TF_IDF_file = TF_IDF_file + "_" + document
        }
        TF_IDF_file = TF_IDF_file + "__.txt"
        val fw = new FileWriter(TF_IDF_file, true)
        for (key <- sortedWordsFrequencyMap_TF_IDF.keys){
            fw.write(key + ": " + sortedWordsFrequencyMap_TF_IDF.getOrElse(key, "-")) 
            fw.write("\n")
        }
        fw.close()
        
        return sortedWordsFrequencyMap_TF_IDF
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

                    case "analyze all similar books" => {
                        var similarBooksArray: Array[String] = Array("gk_chesterton_1.txt", "gk_chesterton_2.txt",
                        "gk_chesterton_3.txt", "gk_chesterton_4.txt", "gk_chesterton_5.txt")
                        
                        // refresh the app and load books into map
                        wordAnalyzerObject.refreshMap()
                        wordAnalyzerObject.refreshArray()
                        for (book <- similarBooksArray){
                            wordAnalyzerObject.importIntoDocumentsMap(book)
                        }
                        
                        // compute word frequencies and save to csv files
                        for (book <- similarBooksArray){
                            // wordAnalyzerObject.refreshArray()
                            wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = book)
                            // wordAnalyzerObject.removeElements()
                            // wordAnalyzerObject.countWordsFrequency()
                            // wordAnalyzerObject.sortWordsMapDescending()
                            // var fileName: String = "word_freq_" + book
                            // wordAnalyzerObject.saveMapFirstN(n = 10000, resultCsvFileName = fileName)
                        }

                        // compute TF-IDFs and save to csv files
                        // wordAnalyzerObject.refreshArray()
                        // for (book <- similarBooksArray){
                        //     wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = book)
                        // }                        
                        // for (book <- similarBooksArray){
                        //     wordAnalyzerObject.computeDocument_TF_IDF(book)
                        // }

                    }

                    case "analyze all different books" => {
                        var differentBooksArray: Array[String] = Array("carter.txt", 
                        "darwin.txt", "king.txt", "nesbitt.txt", "post.txt", "radziwill.txt", "serviss.txt", "shakespeare.txt", 
                        "taylor.txt", "twain.txt")
                        
                        // refresh the app and load books into map
                        wordAnalyzerObject.refreshMap()
                        wordAnalyzerObject.refreshArray()
                        for (book <- differentBooksArray){
                            wordAnalyzerObject.importIntoDocumentsMap(book)
                        }
                        
                        // compute word frequencies and save to csv files
                        for (book <- differentBooksArray){
                            wordAnalyzerObject.refreshArray()
                            wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = book)
                            wordAnalyzerObject.removeElements()
                            wordAnalyzerObject.countWordsFrequency()
                            wordAnalyzerObject.sortWordsMapDescending()
                            var fileName: String = "word_freq_" + book
                            wordAnalyzerObject.saveMapFirstN(n = 10000, resultCsvFileName = fileName)
                        }

                        // compute TF-IDFs and save to csv files
                        wordAnalyzerObject.refreshArray()
                        for (book <- differentBooksArray){
                            wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = book)
                        }                        
                        for (book <- differentBooksArray){
                            wordAnalyzerObject.computeDocument_TF_IDF(book)
                        }

                    }

                    case "analyze mixed books" => {
                        var mixedBooksArray: Array[String] = Array("gk_chesterton_1.txt", "gk_chesterton_2.txt",
                        "gk_chesterton_3.txt", "gk_chesterton_4.txt", "gk_chesterton_5.txt", "nesbitt.txt", "radziwill.txt", 
                        "shakespeare.txt", "taylor.txt", "twain.txt")
                        
                        // refresh the app and load books into map
                        wordAnalyzerObject.refreshMap()
                        wordAnalyzerObject.refreshArray()
                        for (book <- mixedBooksArray){
                            wordAnalyzerObject.importIntoDocumentsMap(book)
                        }
                        
                        // compute word frequencies and save to csv files
                        for (book <- mixedBooksArray){
                            wordAnalyzerObject.refreshArray()
                            wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = book)
                            wordAnalyzerObject.removeElements()
                            wordAnalyzerObject.countWordsFrequency()
                            wordAnalyzerObject.sortWordsMapDescending()
                            var fileName: String = "word_freq_" + book
                            wordAnalyzerObject.saveMapFirstN(n = 10000, resultCsvFileName = fileName)
                        }

                        // compute TF-IDFs and save to csv files
                        wordAnalyzerObject.refreshArray()
                        for (book <- mixedBooksArray){
                            wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = book)
                        }                        
                        for (book <- mixedBooksArray){
                            wordAnalyzerObject.computeDocument_TF_IDF(book)
                        }                        

                    }

                    case "from map import into array" => {
                        // add document from map into array for further actions
                        print("File name: ")
                        var fileName = scala.io.StdIn.readLine()
                        wordAnalyzerObject.addSelectedDocumentIntoArray(file_name = fileName)
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

                    case "compute document TF-IDF" => {
                        print("File name: ")
                        var fileName = scala.io.StdIn.readLine()
                        var sortedWordsFrequencyMap_TF_IDF: Map[String, Double] = wordAnalyzerObject.computeDocument_TF_IDF(fileName)
                        val resultSortedWordsFrequencyMap_TF_IDF: Map[String, Double] = sortedWordsFrequencyMap_TF_IDF.take(10)
                        println(resultSortedWordsFrequencyMap_TF_IDF)
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