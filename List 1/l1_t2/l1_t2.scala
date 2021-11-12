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

    def Reduce(input: List[Tuple2[Int, Int]]): List[Tuple2[Int, List[Int]]] = {
        var inversedList: List[Tuple2[Int, Int]] = List()
        var workingMap: concurrent.Map[Int, List[Int]] = new concurrent.TrieMap()
        var outputList: List[Tuple2[Int, List[Int]]] = List()
        for (row <- input){
            val val1: Int = row._2
            val val2: Int = row._1
            if (val1 != val2){
                if (workingMap.keys.toArray contains val1){
                    var emptyList: List[Int] = List()
                    var oldList: List[Int] = workingMap.getOrElse(val1, emptyList)
                    var newList: List[Int] = oldList :+ val2
                    workingMap.replace(val1, newList)
                } else{
                    workingMap.putIfAbsent(val1, List(val2))
                }
            }
        }
        for (node <- workingMap.keys){
            var emptyList: List[Int] = List()
            var edgesList: List[Int] = workingMap.getOrElse(node, emptyList)
            outputList = outputList :+ (node, edgesList)
        }
        return outputList
    }

}

object MainObject{
    def main(args: Array[String]) = {
        var mapReduceObject = new MapReduce()
        val initialList: List[Tuple2[Int, List[Int]]] = List(
            (1, List(2,3)),
            (3, List(1,5)),
            (2, List(5)),
            (5, List())
        )
        print("\n------: initial list\n")
        for (row <- initialList){
            println(row)
        }
        var workingList: List[Tuple2[Int, Int]] = mapReduceObject.Map(initialList)
        print("\nMap---: working list\n")
        for (pair <- workingList){
            println(pair)
        }
        val outputList: List[Tuple2[Int, List[Int]]] = mapReduceObject.Reduce(workingList)
        print("\nReduce: output list\n")
        for (pair <- outputList){
            println(pair)
        }
    }
}