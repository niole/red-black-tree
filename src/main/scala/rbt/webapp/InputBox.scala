package rbt

import org.scalajs.dom
import dom._
import org.scalajs.dom.html._

import scalatags.JsDom.all._
import scalatags.JsDom.short.*

class InputBox(placeholder: String, header: String) {
  val i: Input = input(*.placeholder:=placeholder).render
  val nodeNumbersContainer: Div = div(id := "node-numbers").render
  val container: Div = div.render
  var renderCallBack: Int => Unit  = (n: Int) => {}

  document.body.appendChild(
    container.appendChild(
      div(
        h1(header),
        div(i),
        nodeNumbersContainer
      ).render
    )
  )

  //could be another event
  i.onkeyup = (e: dom.Event) => {
    val n = i.value
    renderCallBack(n.toInt)
    i.value = ""
  }

  def setRenderCallBack(cb: Int => Unit): Unit = renderCallBack = cb

  def updateVis(allNodesNumbers: Div): Unit = {
    nodeNumbersContainer.innerHTML = ""
    nodeNumbersContainer.appendChild(div(allNodesNumbers).render)
  }

}
