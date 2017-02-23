package rbt

object Main extends App {
  //currently only covers left red and right or left red case
  var n1 = new RedBlackTree(3)
  println(n1)

  n1 = n1.addNode(2)
  println(n1)
  n1 = n1.addNode(1)

  println(n1)

  n1 = n1.addNode(1)

  println(n1)

  n1 = n1.addNode(1)

  println(n1)

}

class RedBlackTree(value: Int) {
  private var root: RootNode = init(value)

  private[this] def init(value: Int): RootNode = new RootNode(value, None, None, "black")

  def addNode(value: Int): RedBlackTree = {
    root = root.addNode(value)
    this
  }

  def getRoot: RootNode = root
}

class RootNode(d: Int, l: Option[TreeNode], r: Option[TreeNode], typ: String) extends TreeNode(d, l, r, typ) {

  override def addNode(value: Int): RootNode = {

    if (value >= data) {
      //add to right

      right = right.map(_.addNode(value)).orElse(Some(new TreeNode(value, None, None, "red")))


      if (hasTwoRedLevels) {
        val onlyRecolor = right.map(_.isRed).getOrElse(false)

        if (!onlyRecolor) {
          //restructuring, sibling black or null
          //is "right right"
          //add a copy of this node as right child of ln, left child of rn is right child of ln

          refactorSubtree(false, onlyRecolor).map(refactored => {
            val refactoredLeftChild = Some(
              new TreeNode(data, refactored.right, left, "red")
            )
            new RootNode(refactored.data, refactored.right, refactoredLeftChild, "black")
          }).get

        }
        else recolorSubtree
      }
      else this
    }
    else {

      left = left.map(_.addNode(value)).orElse(Some(new TreeNode(value, None, None, "red")))

      if (hasTwoRedLevels) {
        val onlyRecolor = right.map(_.isRed).getOrElse(false)

        if (!onlyRecolor) {
          //restructure
          //is "left left"

          refactorSubtree(true, onlyRecolor).map(refactored => {

              val refactoredRightChild = Some(
                new TreeNode(data, refactored.right, right, "red")
              )

              new RootNode(refactored.data, refactored.left, refactoredRightChild, "black")
          }).get
        }
        else recolorSubtree
      }
      else this
    }
  }

  override def cloneAsType(t: String): TreeNode = new RootNode(data, left, right, t)

  override def recolorSubtree: RootNode = {
    new RootNode(
      data,
      left.map(_.cloneAsType("black")),
      right.map(_.cloneAsType("black")),
      "black"
    )
  }

  override def toString: String = {
    val lNode = left.map(ln => ln.toString)
    val rNode = right.map(rn => rn.toString)

    s"RootNode($d, $lNode, $rNode, $t)"
  }

}

class TreeNode(d: Int, l: Option[TreeNode], r: Option[TreeNode], typ: String) {
  val data: Int = d
  var left: Option[TreeNode] = l
  var right: Option[TreeNode] = r
  var t: String = typ

  def recolorSubtree: TreeNode = {
    new TreeNode(
      data,
      left.map(_.cloneAsType("black")),
      right.map(_.cloneAsType("black")),
      "red"
    )
  }

  def cloneAsType(t: String): TreeNode = new TreeNode(data, left, right, t)

  def siblingIsRed(checkLeft: Boolean = false): Boolean = if (checkLeft) left.map(_.isRed).getOrElse(false) else right.map(_.isRed).getOrElse(false)

  def toStringSimple: String = {
    val lNode = left.map(ln => ln.toStringSimple).getOrElse("none")
    val rNode = right.map(rn => rn.toStringSimple).getOrElse("none")

    s"(data: $d - left: $lNode, right: $rNode, type: $t)"
  }

  override def toString: String = {
    val lNode = left.map(ln => ln.toString)
    val rNode = right.map(rn => rn.toString)

    s"TreeNode($d, $lNode, $rNode, $t)"
  }

  def addNode(value: Int): TreeNode = {

    if (value >= data) {
      //add to right

      right = right.map(_.addNode(value)).orElse(Some(new TreeNode(value, None, None, "red")))

      if (hasTwoRedLevels) {
        val onlyRecolor = right.map(_.isRed).getOrElse(false)

        if (!onlyRecolor) {
          //restructuring, sibling black or null
          //is "right right"
          //add a copy of this node as right child of ln, left child of rn is right child of ln

          refactorSubtree(false, onlyRecolor).map(refactored => {
            val refactoredLeftChild = Some(
              new TreeNode(data, refactored.right, left, "red")
            )
            new TreeNode(refactored.data, refactored.right, refactoredLeftChild, "black")
          }).get

        }
        else recolorSubtree
      }
      else this
    }
    else {

      left = left.map(_.addNode(value)).orElse(Some(new TreeNode(value, None, None, "red")))

      if (hasTwoRedLevels) {
        val onlyRecolor = right.map(_.isRed).getOrElse(false)

        if (!onlyRecolor) {
          //restructure
          //is "left left"

          refactorSubtree(true, onlyRecolor).map(refactored => {
              val refactoredRightChild = Some(
                new TreeNode(data, refactored.left, right, "red")
              )

              new TreeNode(refactored.data, refactored.left, refactoredRightChild, "black")
          }).get
        }
        else recolorSubtree
      }
      else this
    }
  }

  /**
   * determines if two levels of red nodes exists in this subtree
   * returns "left" if starts with left child
   * returns "right" if starts with right
   * returns "none" if neither
   */
  def hasTwoRedLevels: Boolean = {
    left.map(leftNode => leftNode.isRed && (leftNode.leftChildIsRed || leftNode.rightChildIsRed)).getOrElse(false) ||
    right.map(rightNode => rightNode.isRed && (rightNode.leftChildIsRed || rightNode.rightChildIsRed)).getOrElse(false)
  }

  /**
   * begins refactorization process of relevant subtree
   * always recolors subnodes
   */
  def refactorSubtree(refactorLeft: Boolean, onlyRecolor: Boolean): Option[TreeNode] = {
    if (refactorLeft) {

      left.map(leftNode => {

        if (leftNode.isRed) {

          if ((leftNode.leftChildIsRed || leftNode.rightChildIsRed) && onlyRecolor) new TreeNode(data, left, right, "black") //recolor middle
          else if (leftNode.rightChildIsRed) {
            //rotate left

            leftNode.right.map(rightNode => {
                val newLeft = left.map(ln => new TreeNode(ln.data, ln.left, rightNode.left, "red"))
                new TreeNode(rightNode.data, newLeft, rightNode.right, "black")
            }).get

          }
          else if (leftNode.leftChildIsRed) new TreeNode(leftNode.data, leftNode.left, leftNode.right, "black") //just recolor, rotate right happens later
          else leftNode //do nothing
        }
        else leftNode
      })
    }
    else {
      right.map(rightNode => {

        if (rightNode.isRed) {

          if ((rightNode.leftChildIsRed || rightNode.rightChildIsRed) && onlyRecolor) new TreeNode(data, left, right, "black") //recolor middle
          else if (rightNode.leftChildIsRed) {
            //rotate right

            rightNode.left.map(leftNode => {
                val newRight = right.map(rn => new TreeNode(rn.data, leftNode.right, rn.right, "red"))
                new TreeNode(leftNode.data, leftNode.left, newRight, "black")
            }).get

          }
          else if (rightNode.rightChildIsRed) new TreeNode(rightNode.data, rightNode.left, rightNode.right, "black") //just recolor, rotate right happens later
          else rightNode //do nothing
        }
        else rightNode
      })
    }
  }

  def leftChildIsRed: Boolean = left.map(_.isRed).getOrElse(false)

  def rightChildIsRed: Boolean = right.map(_.isRed).getOrElse(false)

  def isRed: Boolean = t == "red"
}
