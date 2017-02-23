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

  "A RootNode" should "have accessible attributes: left, right, data, t" in {
    val color = "red"
    val tn = new RootNode(2, Some(new TreeNode(1, None, None, "black")), None, color)

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

  "3 element RedBlackTree" should "rotate when left child and left grandchild are red and right child is None" in {
    val rbt = new RedBlackTree(3)

    assert(rbt.getRoot.data == 3)

    rbt.addNode(2)

    assert(rbt.getRoot.data == 3)
    assert(rbt.getRoot.left.get.data == 2)

    rbt.addNode(1)

    val root = rbt.getRoot

    assert(root.data == 2)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 3)
    assert(root.left.get.t == "red")
    assert(root.right.get.t == "red")
    assert(root.t == "black")
  }

  "3 element RedBlackTree" should "rotate when right child and right grandchild are red and left child is None" in {
    val rbt = new RedBlackTree(1)

    assert(rbt.getRoot.data == 1)

    rbt.addNode(2)

    assert(rbt.getRoot.right.get.data == 2)

    rbt.addNode(3)

    val root = rbt.getRoot

    assert(root.data == 2)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 3)
    assert(root.left.get.t == "red")
    assert(root.right.get.t == "red")
    assert(root.t == "black")
  }

  "3 element RedBlackTree" should "rotate when right child and left grandchild are red and left child is None" in {
    val rbt = new RedBlackTree(1)

    rbt.addNode(3)

    assert(rbt.getRoot.right.get.data == 3)

    rbt.addNode(2)

    val root = rbt.getRoot

    assert(root.data == 2)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 3)
    assert(root.left.get.t == "red")
    assert(root.right.get.t == "red")
    assert(root.t == "black")
  }

}
