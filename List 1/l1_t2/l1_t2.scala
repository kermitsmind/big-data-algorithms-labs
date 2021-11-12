import scala.collection.concurrent
import scala.collection.immutable.ListMap

class MapReduce(){
    def Map(input: List[Tuple2[Int, List[Int]]]): List[Tuple2[Int, Int]] = {
        var outputList: List[Tuple2[Int, Int]] = List()
        for (row <- input){
            val node: Int = row._1
            val edges: List[Int] = row._2
            if (edges.length != 0){
                for (edge <- edges){
                    outputList = outputList :+ (node, edge)
                }
            }else{
                outputList = outputList :+ (node, node)
            }
        }
        return outputList
    }

}

object MainObject{
    def main(args: Array[String]) = {
        var mapReduceObject = new MapReduce()
        val sampleGraph: List[Tuple2[Int, List[Int]]] = List(
            (1, List(2,3)),
            (3, List(1,5)),
            (2, List(5)),
            (5, List())
        )
        var workingList: List[Tuple2[Int, Int]] = mapReduceObject.Map(sampleGraph)
        for (pair <- workingList){
            println(pair)
        }
    }
}