package rbt

import RBT._
import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom._
import window.{clearTimeout, setTimeout}

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
          tree.getAllNumbers(tree.getRoot)
        }).getOrElse("")

      val tm = setTimeout(() => inputBox.updateVis(ns), 500)
    })

  }
}



