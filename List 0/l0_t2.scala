// Resources:
// https://alvinalexander.com/scala/how-to-open-read-text-files-in-scala-cookbook-examples/
// https://stackoverflow.com/questions/14469958/how-to-split-sentence-into-words-separated-by-multiple-spaces
// https://stackoverflow.com/questions/30074109/removing-punctuation-marks-form-text-in-scala-spark
// http://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-filter-filternot-function/
//
//

import scala.io.Source

object MainObject {
    
    def main(args:Array[String]) = {

        
        def importData(file_name: String): Array[String] = {
            var importedWords : Array[String] = Array()
            for (line <- Source.fromFile(file_name).getLines) {
                var words = line.split(" ")
                for (word <- words) {
                    var newWord = word.replaceAll("""[\p{Punct}]""", "")
                    importedWords = importedWords :+ newWord
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
            
        val bookFileName = "l0_t2_text.txt"
        var bookWords = importData(bookFileName)
        
        val stopWordsFileName = "stop_words_english.txt"
        var stopWords = importData(stopWordsFileName)

        var filteredWords = removeElements(bookWords, stopWords)


    }
}