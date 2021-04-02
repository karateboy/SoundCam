package com.apaa

import scalafx.ensemble.commons.DisplayablePage
import scalafx.scene.Node
import scalafx.scene.layout.{Priority, VBox}

object PageDisplayer {
  def showPage(nodeToAdd: DisplayablePage): Node = {
    new VBox {
      vgrow = Priority.Always
      hgrow = Priority.Always
      children = nodeToAdd.getPage
    }
  }
}
