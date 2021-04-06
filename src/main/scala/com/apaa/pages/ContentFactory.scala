package com.apaa.pages

import scalafx.ensemble.commons.DisplayablePage
import scalafx.scene.Node
import scalafx.scene.control.{Label, ScrollPane}
import scalafx.scene.layout.{Priority, StackPane, VBox}

trait ContentPage {
  def getTitle:String
  def getContent: Node
  def getDisplayablePage = {
    ContentFactory.createContent(getTitle, this)
  }
}

object ContentFactory {
  def createContent(title: String, content:ContentPage): DisplayablePage = {

    // Construct content of the samples dynamically
    val sampleNode = content.getContent

    val header = new Label(title) {
      styleClass += "page-header"
    }


    val sampleArea = new StackPane {
      children = sampleNode
      //vgrow = Priority.Sometimes
      vgrow = Priority.Always
    }


    // ScrollPane is applied for borderPane that contains samples
    new DisplayablePage {
      override def getPage: Node = new ScrollPane {
        fitToWidth = true
        fitToHeight = true


        content = new VBox(8) {
          children ++= Seq(header, sampleArea)
          styleClass += "sample-page"
        }

        styleClass += "noborder-scroll-pane"
      }
    }
  }
}
