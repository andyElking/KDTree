import scala.util.Random

case class Point (coords: Array[Double]) {
  def dim: Int = coords.length

  for(x <- coords) {
    require(x >= 0)
  }

  def apply(n: Int) = {
    require(n<dim)
    coords(n)
  }

  def <= (that: Point): Boolean = {
    require(dim == that.dim)
    for (i <- 0 until dim){
      if (apply(i) > that(i)) return false
    }
    true
  }

  /** returns whether the ||this - that||_∞ < dist*/
  def L_∞ (dist: Double, that: Point): Boolean = {
    require(dim == that.dim)
    for (i <- 0 until dim){
      if (math.abs(apply(i) - that(i)) >= dist) return false
    }
    true
  }

  override def toString: String = {
    coords.mkString("[",", ","]")
  }
}

object Point{
  def makeRandomPointBetween(lower_lim: Point, upper_lim: Point): Point = {
    // val rng = new Random(System.nanoTime())
    val dim = lower_lim.dim
    require(lower_lim <= upper_lim)
    val arr = new Array[Double](dim)

    for (i <- 0 until dim) {
      arr(i) = Random.between(lower_lim(i), upper_lim(i))
    }
    return Point(arr)
  }
}