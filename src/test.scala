import scala.util.Random

object test {

  def main(args: Array[String]): Unit = {
    val dim = 3
    val numPointsInTree = 500
    // create a set of random points in R^dim between lower_lim and upper_lim
    val lower_lim: Point = Point(Array.fill[Double](dim)(0.0))
    val upper_lim: Point = Point(Array.fill[Double](dim)(0.99))
    val points = Set.fill[Point](numPointsInTree)(Point.makeRandomPointBetween(lower_lim,upper_lim))

    // create a tree from those points and record the time to build it
    val tr = TimeBlock.time[KDTree]("Time to build tree:")(new KDTree(dim = dim, max_precision = 0.2, points = points))

    // print the internal structure of the tree
    println(tr.printTree)
    println()

    // Search for points in random spheres inside the hypercube and record the time to do so.
    var avg = 0.0
    val numPointsToFind: Int = 10000
    TimeBlock.time[Unit](f"Time to search for $numPointsToFind points:")(
      for (i <- 0 until numPointsToFind) {
        val pt = Point.makeRandomPointBetween(lower_lim, upper_lim)
        val radius = Random.between(0.0,0.8)
        //assert(points.filter(x => pt.L_∞(radius,x)) == tr.find(pt,radius))
        avg += (tr.find(pt, radius).size)/100
        if ((i-1)%100 == 0) print(".")
      }
    )
    avg = avg/100
    println(s"Average number of points found by each call of find: $avg")

  }

}

object TimeBlock {
  /** A timing tool */
  def time[R](text: String)(block: => R): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    println(f"$text  ${t1 - t0}ms")
    result
  }
}
