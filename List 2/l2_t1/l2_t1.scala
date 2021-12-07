import scala.io.Source
import scala.collection.concurrent
import scala.collection.immutable.ListMap

object MainObject{

    def MapFunction(input: String): Tuple2[Int, Int] = {
        var pairString: Array[String] = input.split(" ")
        var node1: Int = pairString(0).toInt
        var node2: Int = pairString(1).toInt
        val output: Tuple2[Int, Int] = (node1, node2)
        return output
    }

    def ReduceFunction(input: Tuple2[Int, Int]): Int = {
        val output: Int = input._1 + input._2
        return output
    }

    def main(args: Array[String]) = {
        
        var initList = Source.fromFile("graph.txt").getLines
        println("initial list:  " + initList.toList)
        var mapList = Source.fromFile("graph.txt").getLines.map(MapFunction)
        println("map:           " + mapList.toList)
        // println("reduce:        " + initialList.map(MapFunction).groupBy(_._1).mapValues(seq => seq.map(_._2).reduce(_ + _)).toMap) // sth works
        //var reduceList = Source.fromFile("graph.txt").getLines.map(MapFunction).reduce(ReduceFunction)
        //println("reduce:        " + reduceList.toList)
    }
}