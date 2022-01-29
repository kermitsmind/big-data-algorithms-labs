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
    // https://www.includehelp.com/scala/multiply-two-matrices.aspx
    // https://www.baeldung.com/scala/find-index-element-in-list
    // https://alvinalexander.com/scala/how-to-format-numbers-commas-international-currency-in-scala/
    // https://en.wikipedia.org/wiki/Matrix_multiplication_algorithm
    // https://stackoverflow.com/questions/10049581/mapping-a-function-over-a-multidimensional-array-in-scala
    // https://stackoverflow.com/questions/59286385/fastest-method-to-multiply-every-element-of-array-with-a-number-in-scala


import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap
import java.io._
import scala.math._
import Array._
import scala.util.matching.Regex
import util.control.Breaks._

object MainObject{

    ////////////////// CRAWL MODE 
    // take name, string array and saves to file with given name
    def writeFile(fileName: String, stringArray: Array[String]): Unit = {
        val fileWriter = new FileWriter(fileName, true)
        val numberOfLinesToWrite: Int = stringArray.length
        var iterator: Int = 1
        for (line <- stringArray){
            if (iterator < numberOfLinesToWrite){
                fileWriter.write(line)
                fileWriter.write("\n")
                iterator = iterator + 1
            }else{
                fileWriter.write(line)
            }
        }
        fileWriter.close()
    }

    // take url, obtain wiki/Topic hrefs and save them to file, returns one link out of the found ones (randomly)
    def crawlPageFromWeb(url: String): String = {
        val pageContent = Source.fromURL(url).mkString
        val pageName = "(?<=https:\\/\\/en.wikipedia.org\\/wiki\\/).*".r.findAllIn(url).next
        // below regex mathes many url's however on wikipedia wikipedia's subpages are linked via 'wiki/Topic' thus below regex won't work as desired
        // val generalHrefPattern = "(https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})".r
        val wikiHrefPattern = "(?<=wiki\\/)(?!.*%|Category|Help|Wikipedia|Special|" +
        "Talk|Privacy_policy|Cookie_statement|Terms_of_Use|Portal|Main_Page|File|Template|(?!.*)/(?!.*))[^\"]*"
        val hrefs = wikiHrefPattern.r.findAllIn(pageContent).toSet.toArray
        writeFile(fileName = ("webPages/" + pageName), stringArray = hrefs)
        val randomNumberGenerator = scala.util.Random
        val randomNumber = randomNumberGenerator.nextInt(hrefs.length)
        return ("https://en.wikipedia.org/wiki/" + hrefs(randomNumber))
    }

    // take initial web page url and number n, crawl n pages and save them to files
    def crawlGivenNumberOfPages(url: String, n: Int): Unit = {
        val crawledPagesListName: String = "crawledPagesList"
        var crawledPagesListArray: Array[String] = Array()
        crawledPagesListArray = crawledPagesListArray.appended("(?<=https:\\/\\/en.wikipedia.org\\/wiki\\/).*".r.findAllIn(url).next)
        var nextUrlToVisit: String = crawlPageFromWeb(url = url)
        var remainingPagesToVisit: Int = n - 1
        for (i <- 1 to remainingPagesToVisit){
            crawledPagesListArray = crawledPagesListArray.appended("(?<=https:\\/\\/en.wikipedia.org\\/wiki\\/).*".r.findAllIn(nextUrlToVisit).next)
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
    
    // take crawled pages list and create transition matrix for them
    def createTransitionMatrixOfCrawledPages(crawledPagesListName: String = "crawledPagesList"): Array[Array[Float]] = {
        // load crawled pages list
        val crawledPagesListArray: Array[String] = readFile(fileName = crawledPagesListName)
        // get number of crawled pages
        val numberOfCrawledPages: Int = crawledPagesListArray.length
        // initialize transition martix of proper size
        var transitionMatrix = Array.ofDim[Float](numberOfCrawledPages, numberOfCrawledPages)
        // iterate through column _,j becuase column has value for a node
        for (j <- 0 to (numberOfCrawledPages - 1)){
            // get current page (node) name
            var pageName: String = crawledPagesListArray(j)
            // get array of the node links and intersect it with the crawled pages array to have only crawled links;
            // by doing so each node contains only links which correspond to other crawled links;
            // let's say we crawled i.e. 5 pages and without intersecting one node may have i.e. 100 links but only 
            // 3 links correspond to other crawled pages thus index of a link corresponding to other crawled page may be 
            // i.e. 57 so it will be out of range for the transition matrix and it will cause an error  
            var pageLinks: Array[String] = crawledPagesListArray.toSet.intersect(readFile(fileName = ("webPages/" + pageName)).toSet).toArray
            // debugging printing
            println("\n\n--------- pageName: " + pageName + "\n----- pageLinks:")
            pageLinks.foreach(x => println("-- " + x))
            // get number of the node links
            var numberOfPageLinks: Int = pageLinks.length
            // compute transition value for links for the given node
            var transitionProbabilityValue: Float = ((1.0)/(numberOfPageLinks)).toFloat
            // iterate through elements in a node
            for (element <- pageLinks){
                // get outNode,currentNode element index
                var elementIndex: Int = crawledPagesListArray.indexOf(element)
                // assign outNode,currentNode transition probability value
                transitionMatrix(elementIndex)(j) = transitionProbabilityValue
            }
        }
        // return transition matrix
        return transitionMatrix
    }

    // get matrix and print it nicely formatted
    def printMatrix(matrix: Array[Array[Float]]): Unit = {
        for (row <- matrix){
            println()
            for (column <- row){
                print(f"$column%1.8f" + " ")
            }
        }
    }

    // multiply two matrices and return the result
    def multiplyMatrices(matrix1: Array[Array[Float]], matrix2: Array[Array[Float]]): Array[Array[Float]] = {
        val numberOfRowsOfMatrix1: Int = matrix1.length
        val numberOfColumnOfMatrix1: Int = matrix1(0).length
        val numberOfColumnOfMatrix2: Int = matrix2(0).length
        var matrix3: Array[Array[Float]] = Array.ofDim[Float](numberOfRowsOfMatrix1, numberOfColumnOfMatrix2)
        var k: Int = 0
        var sum: Float = 0
        for (i <- 0 to (numberOfRowsOfMatrix1 - 1)){
            for (j <- 0 to (numberOfColumnOfMatrix2 - 1)){
                sum = 0
                for (k <- 0 to (numberOfColumnOfMatrix1 - 1)){
                    sum = sum + matrix1(i)(k) * matrix2(k)(j)
                }
                matrix3(i)(j) = sum
            }
        }    
        return matrix3
    }

    // add two matrices of the same size and return the result
    def addMatrices(matrix1: Array[Array[Float]], matrix2: Array[Array[Float]]): Array[Array[Float]] = {
        val numberOfRowsOfMatrix1: Int = matrix1.length
        val numberOfColumnOfMatrix1: Int = matrix1(0).length
        var matrix3: Array[Array[Float]] = Array.ofDim[Float](numberOfRowsOfMatrix1, numberOfColumnOfMatrix1)
        var sum: Float = 0
        for (i <- 0 to (numberOfRowsOfMatrix1 - 1)){
            for (j <- 0 to (numberOfColumnOfMatrix1 - 1)){
                matrix3(i)(j) = matrix1(i)(j) + matrix2(i)(j)
            }
        }    
        return matrix3
    }

    // compare two matrices of the same size and return the result
    def compareMatrices(matrix1: Array[Array[Float]], matrix2: Array[Array[Float]]): Float = {
        val numberOfRowsOfMatrix1: Int = matrix1.length
        val numberOfColumnOfMatrix1: Int = matrix1(0).length
        var differenceVector: Array[Float] = Array()
        var difference: Float = 0
        for (i <- 0 to (numberOfRowsOfMatrix1 - 1)){
            for (j <- 0 to (numberOfColumnOfMatrix1 - 1)){
                difference = (matrix1(i)(j) - matrix2(i)(j)).abs
                differenceVector = differenceVector.appended(difference)
            }
        }    
        return differenceVector.reduceLeft(_ max _).toFloat
    }

    // compute basic Page Rank
    def computePageRank(transitionMatrix: Array[Array[Float]], beta: Float, epsilon: Float): Unit = {
        // get vector V length
        val lengthOfVectorV: Int = transitionMatrix.length
        // fill vector V with initial values (random surfer model: move to every node is equally probable)
        val initialValueOfVectorV: Float = (1.0 / lengthOfVectorV).toFloat
        val valueOfVectorVBeta: Float = ((1.0 - beta.toFloat) / lengthOfVectorV).toFloat
        var temporaryVectorV = Array.ofDim[Float](lengthOfVectorV, 1)
        var vectorVBeta = Array.ofDim[Float](lengthOfVectorV, 1)
        var newTransitionMatrix = transitionMatrix
        var finalVectorVOld = Array.ofDim[Float](lengthOfVectorV, 1)
        newTransitionMatrix = newTransitionMatrix.map(_.map(_ * beta))
        println("+++++++++++++++++++++++++++")
        printMatrix(newTransitionMatrix)
        for (i <- 0 to (lengthOfVectorV - 1)){
            temporaryVectorV(i)(0) = initialValueOfVectorV
            vectorVBeta(i)(0) = valueOfVectorVBeta
            finalVectorVOld(i)(0) = initialValueOfVectorV
        }
        println("")

        printMatrix(temporaryVectorV)
        
        // multiply transition matrix and vector V n times
        var finalVectorVNew = Array.ofDim[Float](lengthOfVectorV, 1)
        var vectorVsDifference: Float = 10.0
        while (vectorVsDifference > epsilon){
            finalVectorVNew = multiplyMatrices(newTransitionMatrix, temporaryVectorV)
            temporaryVectorV = addMatrices(finalVectorVNew, vectorVBeta)
            finalVectorVNew = temporaryVectorV
            println("\n    ---------------------")
            printMatrix(finalVectorVNew)
            vectorVsDifference = compareMatrices(finalVectorVNew, finalVectorVOld)
            println("\n\n    vector's difference: " + vectorVsDifference)
            finalVectorVOld = finalVectorVNew
        }
        // assign every element from vector v to its name from crawled pages list
        // firstly make an array out of vector v matrix
        // then make map (pageName -> pageNameRank) and sort it
        // lastly save sorted map into a file
        var pageRankMap: Map[String, Float] = Map()
        var pageRankMapSorted: Map[String, Float] = Map()
        val crawledPagesListArray: Array[String] = readFile(fileName = "crawledPagesList")
        var vectorV: Array[Float] = Array()
        for (row <- finalVectorVOld){
            for (column <- row){
                vectorV = vectorV.appended(column)
            }
        }
        for (i <- 0 to (vectorV.length - 1)){
            pageRankMap += (crawledPagesListArray(i) -> vectorV(i))
        }
        pageRankMapSorted = ListMap(pageRankMap.toSeq.sortWith(_._2 > _._2):_*)
        val fileWriter = new FileWriter("pageRank", false)
        var lineToWrite: String = "key   value\n"
        fileWriter.write(lineToWrite)
        for (key <- pageRankMapSorted.keys){
            lineToWrite = "\n" + key + "   " + pageRankMap.getOrElse(key, "")
            fileWriter.write(lineToWrite)
        }
        fileWriter.close() 
    }

    // analyse given page
    def analyseGivenPage(pageName: String, transitionMAtrix: Array[Array[Float]]): Unit = {
        val crawledPagesListArray: Array[String] = readFile(fileName = "crawledPagesList")
        var elementIndex: Int = crawledPagesListArray.indexOf(pageName)
        var outLinksNumber: Int = 0
        var outLinksNames: Array[String] = Array()
        var inLinksNumber: Int = 0
        var inLinksNames: Array[String] = Array()
        var counter: Int = 0

        val matrixLength: Int = transitionMAtrix.length
        var matrixCellValue: Float = 0

        val fileWriter = new FileWriter("linkAnalysis", false)
        var lineToWrite: String = ""

        // anlyse out links (pages to which given page is pointing)
        lineToWrite = "\n\n    PAGE: " + pageName + "\n"
        fileWriter.write(lineToWrite)
        print(lineToWrite)

        lineToWrite = "\n\n\n    --------------------\n\nOUT LINK ANALYSIS\n"
        fileWriter.write(lineToWrite)
        print(lineToWrite)
        counter = 0
        for (i_row <- 0 to (matrixLength - 1)){
            matrixCellValue = transitionMAtrix(i_row)(elementIndex)
            if (matrixCellValue > 0.0){
                counter += 1
                lineToWrite = "\n    " + (i_row + 1).toString + ". " + crawledPagesListArray(i_row).toString + " is outlink"
                fileWriter.write(lineToWrite)
                print(lineToWrite)
            }else{
                print("\n    ---")
            }
        }
        lineToWrite = "\n\ntotal no of pages: " + matrixLength.toString + ", outlink no: " + counter.toString
        fileWriter.write(lineToWrite)
        print(lineToWrite) 
        
        // anlyse in links (pages which are pointing to a given page)
        lineToWrite = "\n\n\n    --------------------\n\nIN LINK ANALYSIS\n"
        fileWriter.write(lineToWrite)
        print(lineToWrite)
        counter = 0
        for (j_column <- 0 to (matrixLength - 1)){
            matrixCellValue = transitionMAtrix(elementIndex)(j_column)
            if (matrixCellValue > 0.0){
                counter += 1
                lineToWrite = "\n    " + (j_column + 1).toString + ". " + crawledPagesListArray(j_column).toString + " is inlink"
                fileWriter.write(lineToWrite)
                print(lineToWrite)
            }else{
                print("\n    ---")
            }
        }
        lineToWrite = "\n\n    total no of pages: " + matrixLength.toString + ", inlink no: " + counter.toString
        fileWriter.write(lineToWrite)
        print(lineToWrite)

        fileWriter.close()

    }


    // main function
    def main(args: Array[String]): Unit = {
        var loopCondition: Boolean = true
        breakable{
            while(loopCondition){
                print("\n\nLOOP action: ")
                var loopUserInput = scala.io.StdIn.readLine()
                loopUserInput match {
                    case "crawl" => {
                        print("\n    CRAWL MODE")
                        print("\n    remember to delete data from webPages and crawledPages")
                        print("\n\n    no of pages to crawl: ")
                        var noOfPagesToCrawl = scala.io.StdIn.readLine().toInt
                        print("\n    initial wiki page html: ")
                        var initialWikiPageHtml = scala.io.StdIn.readLine()
                        if (initialWikiPageHtml == ""){
                            initialWikiPageHtml = "https://en.wikipedia.org/wiki/Web_crawler"
                        }
                        crawlGivenNumberOfPages(url = initialWikiPageHtml, n = noOfPagesToCrawl)
                        print("\n    crawling done succesfully")
                        val crawledPagesArray: Array[String] = readFile(fileName = "crawledPagesList")
                        print("\n    crawled pages:\n")
                        for (line <- crawledPagesArray){
                            println("    " + line)
                        }
                    }
                    case "page rank" => {
                        print("\n    PAGE RANK MODE")
                        print("\n    remember to have data from webPages and crawledPages")
                        print("\n\n    beta: ")
                        var beta = scala.io.StdIn.readLine().toFloat
                        print("\n    epsilon: ")
                        var epsilon = scala.io.StdIn.readLine().toFloat
                        var transitionMatrix = createTransitionMatrixOfCrawledPages()
                        print("\n\n    transition matrix\n\n")
                        printMatrix(transitionMatrix)
                        computePageRank(transitionMatrix, 0.90, 0.00001)
                    }
                    case "link analysis" => {
                        print("\n    LINK ANALYSIS MODE")
                        print("\n    remember to have data from webPages and crawledPages")
                        print("\n\n    page name: ")
                        var pageName = scala.io.StdIn.readLine()
                        var transitionMatrix = createTransitionMatrixOfCrawledPages()
                        print("\n\n    transition matrix\n\n")
                        printMatrix(transitionMatrix)
                        analyseGivenPage(pageName, transitionMatrix)                        
                    }
                    case "break" => {
                        loopCondition = false
                    }
                    case _ => {
                        print("\n    not recognized action, try again")
                    }
                }
            }
        }



    }   

}