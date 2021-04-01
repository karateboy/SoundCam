package com.apaa

import scalafx.ensemble.{Ensemble, EnsembleThumbNail, EnsembleTree}
import scalafx.ensemble.EnsembleTree.{exampleListURL, getClass}
import scalafx.ensemble.commons.{ExampleInfo, PageDisplayer, SortUtils}
import scalafx.scene.control.{Button, ContentDisplay, TreeItem}
import scalafx.scene.image.{Image, ImageView}

import java.io.IOException
import scala.collection.immutable.TreeMap

class SoundCamTree {
  private val exampleListPath = ExampleInfo.examplesDir + "example.tree"
  private val exampleListURL = getClass.getResource(exampleListPath)

  def create(): EnsembleTree = new EnsembleTree(createTree(), createThumbnails())

  /**
   * build a map by iterating through the examples folder.
   * This is used in UI
   */
  private def createTree(): Map[String, List[TreeItem[String]]] = {
    val pairs = for ((dirName, examples) <- loadExampleNames()) yield {
      val leaves = for (leafName <- examples) yield {
        new TreeItem(ExampleInfo.formatAddSpaces(leafName))
      }
      dirName -> leaves.toList.sortWith(SortUtils.treeItemSort)
    }
    TreeMap(pairs.toIndexedSeq: _*)
  }

  private def loadExampleNames(): Array[(String, Array[String])] = {

    require(exampleListURL != null, "Failed to locate resource in classpath: " + exampleListPath)

    val lines = scala.io.Source.fromURL(exampleListURL).getLines()

    for (line <- lines.toArray) yield {
      val v = line.split("->")
      assert(v.length == 2)
      val dirName = v.head.trim
      val examples = v(1).split(",").map(_.trim())
      dirName -> examples
    }
  }

  private def createThumbnails() = {
    val pairs = for ((dirName, examples) <- loadExampleNames()) yield {
      val groupName = dirName
      val thumbs = for (leafName <- examples) yield {
        val sampleName = ExampleInfo.formatAddSpaces(leafName)
        val img = new ImageView {
          val filePath = ExampleInfo.thumbnailPath(leafName, groupName)
          val inputStream = this.getClass.getResourceAsStream(filePath)
          if (inputStream == null) {
            throw new IOException("Unable to locate resource: " + filePath)
          }
          image = new Image(inputStream)
        }
        val button = new Button(sampleName, img) {
          prefWidth = 140
          prefHeight = 145
          contentDisplay = ContentDisplay.Top
          styleClass.clear()
          styleClass += "sample-tile"
          /*
          onAction = () => {
            SoundCamApp.splitPane.items.remove(1)
            SoundCamApp.splitPane.items.add(1,
              PageDisplayer.choosePage(groupName + " > " + sampleName))
          }*/
        }
        EnsembleThumbNail(button)
      }
      dirName.capitalize -> thumbs.toList.sortWith(SortUtils.thumbNailsSort)
    }
    TreeMap(pairs.toIndexedSeq: _*)
  }
}
