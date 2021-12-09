import scala.io.Source

object MainObject{

    def MapFunction(input: String): Map[Int, (Int, Int)] = {
        val nodes = input.split("\t").map((x) => x.toInt).toArray
        val map = Map[Int, (Int, Int)]()
        val v0 = nodes(0)
        val v1 = nodes(1)
        //map.addOne(v0, (0, 1))
    }

    // def ReduceFunction(input: Tuple2[Int, Int]): Int = {

    // }

    def main(args: Array[String]) = {
        
        var initList = Source.fromFile("graph.txt").getLines
        println("initial list:  " + initList.toList)
        var mapList = Source.fromFile("graph.txt").getLines.map(MapFunction)
        println("map:           " + mapList.toList)
    }
}