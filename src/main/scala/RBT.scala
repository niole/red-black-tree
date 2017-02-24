package rbt


object RBT {

  def redBlackTree(value: Option[Int]): Option[RedBlackTree] = value.map(new RedBlackTree(_))

  class RedBlackTree(value: Int) {
    private var root: RootNode = init(value)

    def getAllNumbers(node: TreeNode): String = node.left.map(getAllNumbers(_)).getOrElse("")+node.data.toString+node.right.map(getAllNumbers(_)).getOrElse("")

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
          if (!leftChildIsRed) {
            //restructuring, sibling black or null
            //is "right right"
            //add a copy of this node as right child of ln, left child of rn is right child of ln

            rotateRightSubtree.map(refactored => {
              val refactoredLeftChild = Some(
                new TreeNode(data, left, refactored.left, "red")
              )
              new RootNode(refactored.data, refactoredLeftChild, refactored.right, "black")
            }).get

          }
          else recolorSubtree
        }
        else this
      }
      else {

        left = left.map(_.addNode(value)).orElse(Some(new TreeNode(value, None, None, "red")))

        if (hasTwoRedLevels) {
          if (!rightChildIsRed) {
            //restructure

            rotateLeftSubtree.map(refactored => {
              //is "left left"

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
          if (!leftChildIsRed) {
            rotateRightSubtree.map(refactored => {
              //is "right right"

              val refactoredLeftChild = Some(
                new TreeNode(data, left, refactored.left, "red")
              )
              new TreeNode(refactored.data, refactoredLeftChild, refactored.right, "black")
            }).get

          }
          else recolorSubtree
        }
        else this
      }
      else {

        left = left.map(_.addNode(value)).orElse(Some(new TreeNode(value, None, None, "red")))

        if (hasTwoRedLevels) {
          if (!rightChildIsRed) {
            rotateLeftSubtree.map(refactored => {
              //is "left left"

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
     * begins refactorization process of left subtree
     * always recolors subnodes
     */
    def rotateLeftSubtree: Option[TreeNode] = {
      left.map(leftNode => {
        if (leftNode.isRed) {
          if (leftNode.rightChildIsRed) {
            //rotate left

            leftNode.right.map(rightNode => {
                val newLeft = Some(new TreeNode(leftNode.data, leftNode.left, rightNode.left, "red"))
                new TreeNode(rightNode.data, newLeft, rightNode.right, "black")
            }).get

          }
          else if (leftNode.leftChildIsRed) new TreeNode(leftNode.data, leftNode.left, leftNode.right, "black") //just recolor, rotate right happens later
          else leftNode
        }
        else leftNode
      })
    }

    /**
     * begins refactorization process of right subtree
     * always recolors subnodes
     */
    def rotateRightSubtree: Option[TreeNode] = {
      right.map(rightNode => {

        if (rightNode.isRed) {
          if (rightNode.leftChildIsRed) {
            //rotate right

            rightNode.left.map(leftNode => {
                val newRight = right.map(rn => new TreeNode(rn.data, leftNode.right, rn.right, "red"))
                new TreeNode(leftNode.data, leftNode.left, newRight, "black")
            }).get

          }
          else if (rightNode.rightChildIsRed) new TreeNode(rightNode.data, rightNode.left, rightNode.right, "black") //just recolor, rotate left happens later
          else rightNode
        }
        else rightNode
      })
    }

    def leftChildIsRed: Boolean = left.map(_.isRed).getOrElse(false)

    def rightChildIsRed: Boolean = right.map(_.isRed).getOrElse(false)

    def isRed: Boolean = t == "red"
  }

}
