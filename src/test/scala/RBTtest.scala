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

  "balanced 3 element RedBlackTree" should "recolor when left left grandchild added" in {
    val rbt = new RedBlackTree(2)

    rbt.addNode(3)
    rbt.addNode(1)

    var root = rbt.getRoot

    assert(root.data == 2)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 3)
    assert(root.left.get.t == "red")
    assert(root.right.get.t == "red")
    assert(root.t == "black")

    rbt.addNode(0)

    root = rbt.getRoot

    assert(root.data == 2)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 3)
    assert(root.left.get.left.get.data == 0)

    assert(root.left.get.left.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 3 element RedBlackTree" should "recolor when left right grandchild added" in {
    val rbt = new RedBlackTree(3)

    rbt.addNode(4)
    rbt.addNode(1)
    rbt.addNode(2)

    val root = rbt.getRoot


    assert(root.data == 3)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 4)
    assert(root.left.get.right.get.data == 2)

    assert(root.left.get.right.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 3 element RedBlackTree" should "recolor when right right grandchild added" in {
    val rbt = new RedBlackTree(2)

    rbt.addNode(3)
    rbt.addNode(1)
    rbt.addNode(4)

    val root = rbt.getRoot

    assert(root.data == 2)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 3)
    assert(root.right.get.right.get.data == 4)

    assert(root.right.get.right.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 3 element RedBlackTree" should "recolor when right left grandchild added" in {
    val rbt = new RedBlackTree(3)

    rbt.addNode(5)
    rbt.addNode(1)
    rbt.addNode(4)

    val root = rbt.getRoot

    assert(root.data == 3)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 5)
    assert(root.right.get.left.get.data == 4)

    assert(root.right.get.left.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 4 element RedBlackTree" should "rotate when right left left grandchild added" in {
    val rbt = new RedBlackTree(3)

    rbt.addNode(6)
    rbt.addNode(1)
    rbt.addNode(5)
    rbt.addNode(4)

    val root = rbt.getRoot

    assert(root.data == 3)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 5)
    assert(root.right.get.left.get.data == 4)
    assert(root.right.get.right.get.data == 6)

    assert(root.right.get.left.get.t == "red")
    assert(root.right.get.right.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 4 element RedBlackTree" should "rotate when right left right grandchild added" in {
    val rbt = new RedBlackTree(3)

    rbt.addNode(1)
    rbt.addNode(4)
    rbt.addNode(5)
    rbt.addNode(6)

    val root = rbt.getRoot

    assert(root.data == 3)
    assert(root.left.get.data == 1)
    assert(root.right.get.data == 5)
    assert(root.right.get.left.get.data == 4)
    assert(root.right.get.right.get.data == 6)

    assert(root.right.get.left.get.t == "red")
    assert(root.right.get.right.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 4 element RedBlackTree" should "rotate when left left left grandchild added" in {
    val rbt = new RedBlackTree(5)

    rbt.addNode(6)
    rbt.addNode(4)
    rbt.addNode(3)
    rbt.addNode(1)

    val root = rbt.getRoot

    assert(root.data == 5)
    assert(root.left.get.data == 3)
    assert(root.right.get.data == 6)
    assert(root.left.get.left.get.data == 1)
    assert(root.left.get.right.get.data == 4)

    assert(root.left.get.left.get.t == "red")
    assert(root.left.get.right.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 4 element RedBlackTree" should "rotate when left left right grandchild added" in {
    val rbt = new RedBlackTree(5)

    rbt.addNode(6)
    rbt.addNode(3)
    rbt.addNode(1)
    rbt.addNode(2)

    val root = rbt.getRoot

    assert(root.data == 5)
    assert(root.left.get.data == 2)
    assert(root.right.get.data == 6)
    assert(root.left.get.left.get.data == 1)
    assert(root.left.get.right.get.data == 3)

    assert(root.left.get.left.get.t == "red")
    assert(root.left.get.right.get.t == "red")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 5 element RedBlackTree" should "recolor when grandchild added to 0th leaf, left side" in {
    val rbt = new RedBlackTree(5)

    rbt.addNode(6)
    rbt.addNode(3)
    rbt.addNode(1)
    rbt.addNode(4)
    rbt.addNode(0)

    val root = rbt.getRoot

    assert(root.data == 5)
    assert(root.left.get.data == 3)
    assert(root.right.get.data == 6)
    assert(root.left.get.left.get.data == 1)
    assert(root.left.get.right.get.data == 4)
    assert(root.left.get.left.get.left.get.data == 0)

    assert(root.left.get.left.get.left.get.t == "red")
    assert(root.left.get.left.get.t == "black")
    assert(root.left.get.right.get.t == "black")
    assert(root.left.get.t == "red")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 5 element RedBlackTree" should "recolor when grandchild added to 0th leaf, right side" in {
    val rbt = new RedBlackTree(5)

    rbt.addNode(6)
    rbt.addNode(3)
    rbt.addNode(1)
    rbt.addNode(4)
    rbt.addNode(1)

    val root = rbt.getRoot

    assert(root.data == 5)
    assert(root.left.get.data == 3)
    assert(root.right.get.data == 6)
    assert(root.left.get.left.get.data == 1)
    assert(root.left.get.right.get.data == 4)
    assert(root.left.get.left.get.right.get.data == 1)

    assert(root.left.get.left.get.right.get.t == "red")
    assert(root.left.get.left.get.t == "black")
    assert(root.left.get.right.get.t == "black")
    assert(root.left.get.t == "red")
    assert(root.right.get.t == "black")
    assert(root.t == "black")

  }

  "balanced 5 element RedBlackTree" should "recolor when great grandchild added to 2nd leaf, left side" in {
    val rbt = new RedBlackTree(3)

    rbt.addNode(2)
    rbt.addNode(6)
    rbt.addNode(5)
    rbt.addNode(7)
    rbt.addNode(4)

    val root = rbt.getRoot

    assert(root.data == 3)
    assert(root.left.get.data == 2)
    assert(root.right.get.data == 6)
    assert(root.right.get.left.get.data == 5)
    assert(root.right.get.left.get.left.get.data == 4)
    assert(root.right.get.right.get.data == 7)

    assert(root.t == "black")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "red")
    assert(root.right.get.left.get.t == "black")
    assert(root.right.get.left.get.left.get.t == "red")
    assert(root.right.get.right.get.t == "black")

  }

  "balanced 5 element RedBlackTree" should "recolor when great grandchild added to 2nd leaf, right side" in {
    val rbt = new RedBlackTree(3)

    rbt.addNode(2)
    rbt.addNode(6)
    rbt.addNode(5)
    rbt.addNode(7)
    rbt.addNode(5)

    val root = rbt.getRoot

    assert(root.data == 3)
    assert(root.left.get.data == 2)
    assert(root.right.get.data == 6)
    assert(root.right.get.left.get.data == 5)
    assert(root.right.get.left.get.right.get.data == 5)
    assert(root.right.get.right.get.data == 7)

    assert(root.t == "black")
    assert(root.left.get.t == "black")
    assert(root.right.get.t == "red")
    assert(root.right.get.left.get.t == "black")
    assert(root.right.get.left.get.right.get.t == "red")
    assert(root.right.get.right.get.t == "black")

  }

}
