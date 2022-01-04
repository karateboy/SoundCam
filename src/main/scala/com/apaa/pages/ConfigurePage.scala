package com.apaa.pages


import com.apaa.SoundCamApp.stage
import javafx.util.converter.IntegerStringConverter
import scalafx.geometry.Pos.Center
import scalafx.scene.Node
import scalafx.scene.control._
import scalafx.scene.layout.{GridPane, HBox}
import scalafx.stage.DirectoryChooser
import scalafx.util.converter.DoubleStringConverter

import java.io.File

class ConfigurePage extends ContentPage {
  override def getContent: Node = {
    val grid: GridPane = new GridPane()
    grid.setAlignment(Center)
    grid.setHgap(10)
    grid.setVgap(10)
    //grid.setPadding(Insets(25))

    grid.add(Label("輸出目錄:"), 0, 1)

    val outputDir = new TextField()
    outputDir.setPrefWidth(200)
    grid.add(outputDir, 1, 1)
    val browseBtn = new Button("..."){
      onAction = (evt)=>{
        val dirChooser = new DirectoryChooser()
        dirChooser.setTitle("選擇輸出目錄")
        val dir: File = dirChooser.showDialog(stage)
        if(dir != null && dir.isDirectory){
          outputDir.text = dir.getAbsolutePath
        }
      }
    }
    grid.add(browseBtn, 2, 1)

    grid.add(Label("噪音觸發值(dB)"), 0, 2)
    val triggerDb = new TextField()
    triggerDb.textFormatter = new TextFormatter[Double](new DoubleStringConverter, 0.0, (change: TextFormatter.Change) =>{
      val txt = change.getControlNewText
      if(txt.toDoubleOption.nonEmpty)
        change
      else
        null
    })
    grid.add(triggerDb, 1, 2)

    grid.add(Label("前觸發(秒)"), 0, 3)
    val preTrigger = new TextField()
    preTrigger.textFormatter = new TextFormatter[Integer](new IntegerStringConverter, 5, (change: TextFormatter.Change) =>{
      val txt = change.getControlNewText
      if(txt.toIntOption.nonEmpty)
        change
      else
        null
    })
    grid.add(preTrigger, 1, 3)

    grid.add(Label("觸發後記錄(秒)"), 0, 4)
    val postTrigger = new TextField()
    postTrigger.textFormatter = new TextFormatter[Integer](new IntegerStringConverter, 5, (change: TextFormatter.Change) =>{
      val txt = change.getControlNewText
      if(txt.toIntOption.nonEmpty)
        change
      else
        null
    })
    grid.add(postTrigger, 1, 4)
    grid.add(Label("噪音影像放大模式"), 0, 5)
    val scaleModeGroup = new ToggleGroup()
    val radioAuto = new RadioButton("Auto")
    radioAuto.setToggleGroup(scaleModeGroup)
    val radioSmart = new RadioButton("Smart")
    radioSmart.setToggleGroup(scaleModeGroup)
    radioAuto.setSelected(true)
    grid.add(new HBox(radioAuto, radioSmart), 1, 5, 2, 1)
    grid
  }

  override def getTitle: String = "設定"
}
