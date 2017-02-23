package rbt

import org.scalatest._


class RBTtest extends FlatSpec {

  "A TreeNode" should "have accessible attributes: left, right, data, t" in {
    val color = "red"
    val tn = new TreeNode(2, Some(new TreeNode(1, None, None, "black")), None, color)

    assert(tn.data == 2)
    assert(tn.left.get.data == 1)
    assert(tn.right == None)
    assert(tn.t == color)
  }

  "initialized RedBlackTree" should "contain one black RootNode" in {
    val color = "black"
    var rbt = new RedBlackTree(3)
    val root = rbt.getRoot

    assert(root.data == 3)
    assert(root.left == None)
    assert(root.right == None)
    assert(root.t == color)
  }
}
