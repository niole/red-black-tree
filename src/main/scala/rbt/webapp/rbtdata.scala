package rbt

import RBT._
import scala.scalajs.js.JSApp
import scalatags.JsDom.TypedTag
import scalatags.JsDom.all._
import org.scalajs.dom
import dom._
import dom.html._
import window.setTimeout

object RBTData extends JSApp {
  def main(): Unit = {
    //map over the red black tree and return stuff
    //go to leaves, and position everything from there
    //mvp of the mvp: a string

    var rbt = redBlackTree(None)

    val inputBox: InputBox = new InputBox("enter a number", "Add a Node")

    inputBox.setRenderCallBack((number: Int) => {
      rbt = rbt.map(_.addNode(number)).orElse(Some(new RedBlackTree(number)))

      val ns = rbt.map(tree => {
          tree.getAllNumbers(tree.getRoot, getNumberElement, "root").render
        }).getOrElse(div().render)

      val tm = setTimeout(() => inputBox.updateVis(ns), 500)
    })

    def getNumberElement(node: TreeNode): TypedTag[Div] = {
      val t = node.t
      val data = node.data
      div(cls:=s"letter-node $t", s"$data")
    }

  }
}

