import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap

object MainObject{

     //                                             node, 1
    def MapFunction(input: Tuple2[Int, Int]): Tuple2[Int, Int] = {
        val output: Tuple2[Int, Int] = (input._1, 1) // node, 1
        return output
    }

    def main(args: Array[String]) = {
        
        // import initial list from a file and append to a list variable
        var initialList: List[Tuple2[Int, Int]] = List()
        for (line <- Source.fromFile("graph.txt").getLines) {
            var pairString: Array[String] = line.split(" ")
            var pair: Tuple2[Int, Int] = (pairString(0).toInt, pairString(1).toInt)
            initialList = initialList :+ pair
            // println(initialList)
        }

        // 
        println("initial list:  " + initialList)
        println("map:           " + initialList.map(MapFunction))
        //println("reduce:        " + initialList.map(MapFunction).groupBy(_._1).mapValues(seq => seq.map(_._2).reduce(_ + _)).toMap)
        println("reduce experiment:        " + initialList.map(MapFunction).groupBy(_._1)     )
    }
}