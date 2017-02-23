object Main extends App {
  //currently only covers left red and right or left red case
  val n1 = new TreeNode(3, None, None, None, "black")
  n1.addNode(2)
  n1.addNode(1)

  println(n1.toStringSimple)
}

class TreeNode(d: Int, p: Option[TreeNode], l: Option[TreeNode], r: Option[TreeNode], typ: String) {
  val data: Int = d
  var parent: Option[TreeNode] = p
  var left: Option[TreeNode] = l
  var right: Option[TreeNode] = r
  var t: String = typ

  def toStringSimple: String = {
    val lNode = left.map(ln => ln.toStringSimple).getOrElse("none")
    val rNode = right.map(rn => rn.toStringSimple).getOrElse("none")

    s"(data: $d - left: $lNode, right: $rNode, type: $t)"
  }

  override def toString: String = {
    val pNode = parent.map(prent => prent.toString)
    val lNode = left.map(ln => ln.toString)
    val rNode = right.map(rn => rn.toString)

    s"TreeNode($d, $pNode, $lNode, $rNode, $t)"
  }

  def cloneTreeNode: TreeNode = new TreeNode(data, parent, left, right, t)

  def addNode(value: Int): TreeNode = {
    if (value >= data) {
      //add to right
      //orElse is the perfect way to assign default values to Options

      right = right.map(_.addNode(value)).orElse(Some(new TreeNode(value, Some(cloneTreeNode), None, None, "red").refactor))
    }
    else {
      left = left.map(_.addNode(value)).orElse(Some(new TreeNode(value, Some(cloneTreeNode), None, None, "red").refactor)) //TODO get rid of this parent clone, too much data, rely on recursion
    }


    this
  }

  def refactor: TreeNode = {
    //if child and grandchild are red, refactor
    val side = twoRedLevels

    println("side")
    println(side)
    println(this.toString)

    if (side == "left") {
      var updatedChild = getRefactorableChild(true)

      if (right.map(_.isRed).getOrElse(false)) {
        //if sibling is red in the middle of a recolor
        //updatedChild is already be black

        t = "red"
        left = updatedChild
        right = right.map(rightNode => new TreeNode(rightNode.data, Some(cloneTreeNode), rightNode.left, rightNode.right, "black"))
      } else if (right.map(!_.isRed).getOrElse(false)) {
        //sibling is black or null
        //in the middle of a restructure
        //by this time we should already have done a left rotation if necessary
        //only do right rotation


        //update right child of updated node to be (this), and left child to be updateChild's old right child
        updatedChild = updatedChild.map(updated => {
          updated.right = Some(new TreeNode(data, updatedChild, updated.right, right, "black"))
          updated
        })

        //point parent at new middle node
        parent = parent.map(pNode => {
          if (pNode.left.map(_ == this).getOrElse(false)) {
            pNode.left = updatedChild
          }
          else if (pNode.right.map(_ == this).getOrElse(false)) {
            pNode.right = updatedChild
          }
          else {
            println("equality between nodes isn't working/I don't understand it. check refactor")
            pNode
          }

          pNode
        })

        updatedChild.map(updated => {
          updated.parent = parent.map(_.refactor)
          updated
        })

        updatedChild.get
      }

    }
    else if (side == "right") {
    }

    //keep going up the tree
    //will naturally stop bc is Option

    parent = parent.map(_.refactor)

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
   * gets refactorable child and begins refactorization process if possible
   */
  def getRefactorableChild(getLeft: Boolean = false): Option[TreeNode] = {
    if (getLeft) {
      left.map(leftNode => {
        if (leftNode.isRed) {

          if (leftNode.leftChildIsRed) new TreeNode(data, parent, left, right, "black") //recolor
          else if (leftNode.rightChildIsRed) {
            //rotate left
            var rightGrandChild = leftNode.right.map(rightNode => new TreeNode(rightNode.data, Some(cloneTreeNode), None, rightNode.right, "black"))

            rightGrandChild = rightGrandChild.map(rightGNode => {
                rightGNode.left = Some(
                  new TreeNode(leftNode.data, rightGrandChild, leftNode.left, leftNode.right.flatMap(_.left), "red")
                )
                rightGNode
              })

            rightGrandChild
          }
          else leftNode
        }
        leftNode
      })
    }
    else {
      //TODO FIX, right now just making type check

      right.map(rightNode => {
        if (rightNode.isRed) {

          if (rightNode.rightChildIsRed) new TreeNode(data, parent, left, right, "black")
          else if (rightNode.leftChildIsRed) {
            //TODO FIX rotate right
            rightNode
          }
        }

        rightNode
      })
    }
  }

  def leftChildIsRed: Boolean = left.map(_.isRed).getOrElse(false)

  def rightChildIsRed: Boolean = right.map(_.isRed).getOrElse(false)

  def isRed: Boolean = t == "red"
}
