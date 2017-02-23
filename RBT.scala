object Main extends App {
  //currently only covers left red and right or left red case
  var n1 = new RootNode(3, None, None, "black")
  n1 = n1.addNode(2)
  n1 = n1.addNode(1)

  println(n1.toStringSimple)
}

class RootNode(d: Int, l: Option[TreeNode], r: Option[TreeNode], typ: String) extends TreeNode(d, l, r, typ) {

  override def addNode(value: Int): RootNode = {
    val side = twoRedLevels
    val onlyRecolor = right.map(_.isRed).getOrElse(false)

    if (value >= data) {
      //add to right

      if (side != "none" && !onlyRecolor) {
        //restructuring, sibling black or null
        //is "right right"
        //add a copy of this node as right child of ln, left child of rn is right child of ln

        right.map(rn => {
            val added = rn.addNode(value)
            val refactored = added.refactor(side, onlyRecolor)
            val refactoredLeftChild = Some(
              new TreeNode(data, refactored.right, left, "red")
            )

            new RootNode(refactored.data, refactored.right, refactoredLeftChild, "black")
        }).getOrElse(new TreeNode(value, None, None, "red"))

      }
      else {
        //just recoloring, maintain current shape from outset
        right = right.map(rn => {
            val added = rn.addNode(value)
            added.refactor(side, onlyRecolor)
        }).orElse(Some(new TreeNode(value, None, None, "red")))
      }
    }
    else {
      if (side != "none" && !onlyRecolor) {
        //restructure
        //is "left left"

        left.map(ln => {
            val added = ln.addNode(value)
            val refactored = added.refactor(side, onlyRecolor)

            val refactoredRightChild = Some(
              new TreeNode(data, refactored.left, right, "red")
            )
            new RootNode(refactored.data, refactored.left, refactoredRightChild, "black")
        }).getOrElse(new RootNode(value, None, None, "red"))
      }
      else {
        //recoloring
        left = left.map(ln => {
            val added = ln.addNode(value)
            added.refactor(side, onlyRecolor)
        }).orElse(Some(new TreeNode(value, None, None, "red")))
      }

    }

    this
  }

}

class TreeNode(d: Int, l: Option[TreeNode], r: Option[TreeNode], typ: String) {
  val data: Int = d
  var left: Option[TreeNode] = l
  var right: Option[TreeNode] = r
  var t: String = typ

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
    val side = twoRedLevels
    val onlyRecolor = right.map(_.isRed).getOrElse(false)

    if (value >= data) {
      //add to right

      if (side != "none" && !onlyRecolor) {
        //restructuring, sibling black or null
        //is "right right"
        //add a copy of this node as right child of ln, left child of rn is right child of ln

        right.map(rn => {
            val added = rn.addNode(value)
            val refactored = added.refactor(side, onlyRecolor)
            val refactoredLeftChild = Some(
              new TreeNode(data, refactored.right, left, "red")
            )

            new TreeNode(refactored.data, refactored.right, refactoredLeftChild, "black")
        }).getOrElse(new TreeNode(value, None, None, "red"))

      }
      else {
        //just recoloring, maintain current shape from outset
        right = right.map(rn => {
            val added = rn.addNode(value)
            added.refactor(side, onlyRecolor)
        }).orElse(Some(new TreeNode(value, None, None, "red")))
      }
    }
    else {
      if (side != "none" && !onlyRecolor) {
        //restructure
        //is "left left"

        left.map(ln => {
            val added = ln.addNode(value)
            val refactored = added.refactor(side, onlyRecolor)

            val refactoredRightChild = Some(
              new TreeNode(data, refactored.left, right, "red")
            )
            new TreeNode(refactored.data, refactored.left, refactoredRightChild, "black")
        }).getOrElse(new TreeNode(value, None, None, "red"))
      }
      else {
        //recoloring
        left = left.map(ln => {
            val added = ln.addNode(value)
            added.refactor(side, onlyRecolor)
        }).orElse(Some(new TreeNode(value, None, None, "red")))
      }

    }

    this
  }

  def refactor(side: String, onlyRecolor: Boolean): TreeNode = {
    //if child and grandchild are red, refactor
    //need RootNode and TreeNode versions

    if (side == "left") {

      var updatedChild = refactorSubtree(true, onlyRecolor) //subtree recolored properly, first rotatation has happened if necessary

      if (onlyRecolor) {
        //recolor self and right child

        new TreeNode(
          data,
          updatedChild,
          right.map(rightNode => new TreeNode(rightNode.data, rightNode.left, rightNode.right, "black")),
          "red"
        )
      }
      else {
        //sibling is black or null
        //by this time we should already have done a left rotation if necessary
        //only do right rotation

        updatedChild.get
      }

    }
    else if (side == "right") {
      var updatedChild = refactorSubtree(false, onlyRecolor)

      if (onlyRecolor) {
        //recolor self and left child

        new TreeNode(
          data,
          left.map(leftNode => new TreeNode(leftNode.data, leftNode.left, leftNode.right, "black")),
          updatedChild,
          "red"
        )
      } else {
        //sibling is black or null
        //by this time we should already have done a left rotation if necessary
        //only do right rotation

        updatedChild.get
      }

    }

    this
  }

  /**
   * determines if two levels of red nodes exists in this subtree
   * returns "left" if starts with left child
   * returns "right" if starts with right
   * returns "none" if neither
   */
  def twoRedLevels: String = {
    if (left.map(leftNode => leftNode.isRed && (leftNode.leftChildIsRed || leftNode.rightChildIsRed)).getOrElse(false)) "left"
    else if (right.map(rightNode => rightNode.isRed && (rightNode.leftChildIsRed || rightNode.rightChildIsRed)).getOrElse(false)) "right"
    else "none"
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
          else if (leftNode.leftChildIsRed) new TreeNode(data, left, right, "black") //just recolor, rotate right happens later
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
          else if (rightNode.rightChildIsRed) new TreeNode(data, left, right, "black") //just recolor, rotate right happens later
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
