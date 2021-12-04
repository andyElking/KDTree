
// TO DO:
/*
* Function to add in objects
* Function that returns refs of objects hovered over
* what else?
* */
/** An implementation of n-dimensional hypercube K-D tree.
 * @param side_length - the side length of the hypercube in which all points lie
 * @param dim - dimensionality of the space
 * @param max_precision - how finely will the space be subdivided
 * @param points - the initial set of points in the space, more can be added later
 * */
class KDTree (side_length: Double = 1.0, dim: Int = 2, max_precision: Double, points: Set[Point]){
  require(max_precision > 0)

  private val lower_lim: Point = Point(Array.fill[Double](dim)(0.0))
  private val upper_lim: Point = Point(Array.fill[Double](dim)(side_length))

  private val root: Node = Node.createNode(lower_lim, upper_lim, 0, points, "/")

  /** Insert a new point into the data structure. */
  def insert(pt: Point): Unit = {
    root.insert(pt)
  }

  /** Returns the set of points within radius around x.*/
  def find(x: Point, radius: Double): Set[Point] = root.find(x, radius)

  /** Prints out the structure of the tree in a human-readable way. */
  def printTree = root.printSubtree

  private trait Node{
    val id: String // a binary string representing the parenthood up the tree
    val lower_lim: Point
    val upper_lim: Point
    def find(x: Point, radius: Double): Set[Point]
    def points: Set[Point]
    def insert(pt: Point): Unit
    def printSubtree: String
  }

  private class TreeNode(override val lower_lim: Point, override val upper_lim: Point, val splitDim: Int, pts: Set[Point], val id: String) extends Node {

    //assert(lower_lim <= upper_lim)
    //assert(splitDim < dim)

    // compute child boundaries
    val x0: Double = lower_lim(splitDim)
    val x2: Double = upper_lim(splitDim)
    val x1: Double = (x0 + x2)/2.0  // where to split the children along

    val upper_lim1_coords: Array[Double] = upper_lim.coords.clone()
    upper_lim1_coords(splitDim) = x1
    val next_split_dim: Int = (splitDim + 1)%dim

    val lower_lim2_coords: Array[Double] = lower_lim.coords.clone()
    lower_lim2_coords(splitDim) = x1

    // divide the points among the children
    val points1: Set[Point] = pts.filter(pt => pt(splitDim) <= x1)
    val points2: Set[Point] = pts.filter(pt => pt(splitDim) > x1)

    //assert(points1.union(points2) == pts)

    // create children
    val child1: Node = Node.createNode(lower_lim, Point(upper_lim1_coords), next_split_dim, points1, id + "0")
    val child2: Node = Node.createNode(Point(lower_lim2_coords), upper_lim, next_split_dim, points2, id + "1")

    /** Returns any points in this node that fall within the radius.
     * It checks which of the children the interval [x-radius,x+radius] intersects
     * and calls find inside the children.*/
    override def find(x: Point, radius: Double): Set[Point] = {
      val x_split = x(splitDim) // the relevant dimension of x
      val x_lower = x_split - radius
      val x_upper = x_split + radius
      // there is intersection with child1 iff x_lower <= x1 && x_upper >= x0
      val intersects_child1 = x_lower <= x1 && x_upper >= x0
      val intersects_child2 = x_lower <= x2 && x_upper >= x1
      var res: Set[Point] = Set[Point]()
      if(intersects_child1) res = res.union(child1.find(x, radius))
      if(intersects_child2) res = res.union(child2.find(x, radius))
      res
    }

    override def insert(pt: Point): Unit = {
      //assert(lower_lim <= pt && pt <= upper_lim)
      val x_split = pt(splitDim)
      if (x_split <= x1) {
        child1.insert(pt)
      } else {
        child2.insert(pt)
      }
    }

    override def toString: String = {
      s"TreeNode $id. From $lower_lim to $upper_lim"
    }

    def printSubtree: String = "|  "*(id.length - 1) + toString + "\n" + child1.printSubtree + child2.printSubtree

    override def points: Set[Point] = child1.points.union(child2.points)
  }


  private class LeafNode (override val lower_lim: Point, override val upper_lim: Point, var pts: Set[Point], val id: String) extends Node {

    //assert(pts.forall(pt => lower_lim <= pt && pt <= upper_lim))

    override def find(x: Point, radius: Double): Set[Point] = {
      pts.filter(pt => x.L_∞(radius, pt))
    }

    override def insert(pt: Point): Unit = {
      //assert(lower_lim <= pt && pt <= upper_lim)
      pts = pts + pt
    }

    override def printSubtree: String = "|  "*(id.length - 1) + toString + pts.mkString("  {", ", ", "}") + "\n"

    override def points: Set[Point] = pts

    override def toString: String = {
      s"LeafNode $id. From $lower_lim to $upper_lim"
    }

  }

  /** Companion object. Used for creating children heh. */
  private object Node {

    def createNode(lower_lim: Point, upper_lim: Point, splitDim: Int, pts: Set[Point], id: String): Node = {
      // checks whether the node should be a LeafNode or a TreeNode
      if(pts.size <= 1 || lower_lim.L_∞(max_precision,upper_lim)) {
        new LeafNode(lower_lim, upper_lim, pts, id)
      } else {
        new TreeNode(lower_lim, upper_lim, splitDim, pts, id)
      }
    }
  }
}


