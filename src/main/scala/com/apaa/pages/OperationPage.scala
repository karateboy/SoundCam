package com.apaa.pages

import akka.actor.typed.ActorRef
import com.apaa.SoundCamClient._
import com.apaa.{DeviceState, SoundCamApp, SoundCamClient, SoundCamInfoHandler}
import org.slf4j.LoggerFactory
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.chart.{LineChart, NumberAxis, XYChart}
import scalafx.scene.control.{Button, Label, TextField}
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{BorderPane, GridPane, HBox}

import java.time.LocalDateTime

class OperationPage extends ContentPage {
  val logger = LoggerFactory.getLogger(this.getClass)

  override def getContent: Node = {
    val toolbar = getToolbar
    val imageView = new ImageView()
    imageView.image = new Image("/scalafx/ensemble/images/boat.jpg")
    SoundCamInfoHandler.setVideoSink(imageView)
    val borderPane = new BorderPane()
    borderPane.setTop(toolbar)

    val stack = getDbaPane

    val grid = new GridPane()
    grid.setAlignment(Pos.TopLeft)
    grid.setHgap(10)
    grid.setVgap(10)
    grid.setPadding(Insets(25))
    grid.add(imageView, 0, 0)
    grid.add(stack, 1, 0)
    val acoustImage = new ImageView("/scalafx/ensemble/images/animals-200x200/animal2.jpg")
    SoundCamInfoHandler.setAcousticSink(acoustImage)
    grid.add(acoustImage, 0, 1)
    grid.add(getSpectrumPane, 1, 1)

    borderPane.setCenter(grid)
    borderPane
  }

  private def getToolbar = {
    val client: ActorRef[SoundCamClient.Command] = SoundCamApp.soundCamClient
    val btnPrepare = new Button("Prepare") {
      onAction = (_) => {
        // client !SoundCamClient.StartProcedure
        val now = LocalDateTime.now()
        val currentDateTime = CurrentDateTime(now.getYear, now.getMonthValue, now.getDayOfMonth,
          now.getHour, now.getMinute, now.getSecond, false)
        val dataObjects = Seq(
          Distance(100),
          FrequencyRange(5623, 11200),
          CameraResolution(640, 480),
          VideoFrameRate(30),
          AcousticFrameRate(30),
          currentDateTime
        )
        client ! SoundCamClient.WriteDataObject(dataObjects)
        client ! SoundCamClient.PrepareState(DeviceState.Measuring)
      }
    }
    val btnStart = new Button("Start") {
      onAction = (_) => {
        client ! SoundCamClient.StartProcedure
      }
    }

    val btnStop = new Button("Stop") {
      onAction = (_) => {
        client ! SoundCamClient.StopProcedure
      }
    }

    val toolbar = new HBox(btnPrepare, btnStart, btnStop)
    toolbar.setPadding(Insets(10))
    val imageView = new ImageView()
    imageView.image = new Image("/scalafx/ensemble/images/boat.jpg")
    SoundCamInfoHandler.setVideoSink(imageView)
    toolbar.setPadding(Insets(10))
    toolbar
  }

  def getDbaPane = {
    val grid = new GridPane()
    grid.setAlignment(Pos.Center)
    grid.setHgap(10)
    grid.setVgap(10)
    grid.add(new Label("Global dB(A)"), 0, 0)
    val globalDBa = new TextField()
    globalDBa.setPrefWidth(40)
    globalDBa.setEditable(false)
    grid.add(globalDBa, 1, 0)
    grid.add(new Label("Local dB(A)"), 0, 1)
    val localDBa = new TextField()
    localDBa.setPrefWidth(40)
    localDBa.setEditable(false)
    SoundCamInfoHandler.setDbaTextField(globalDBa, localDBa)
    grid.add(localDBa, 1, 1)
    grid.setPadding(Insets(10))
    grid
  }

  def getSpectrumPane = {
    val xAxis = NumberAxis("頻率 Hz")
    val yAxis = NumberAxis("聲壓 dB(A)")

    // Helper function to convert a tuple to `XYChart.Data`
    val toChartData = (xy: (Double, Double)) => XYChart.Data[Number, Number](xy._1, xy._2)

    val series1 = new XYChart.Series[Number, Number] {
      name = "Series 1"
      data = Seq(
        (0.0, 1.0),
        (1.2, 1.4),
        (2.2, 1.9),
        (2.7, 2.3),
        (2.9, 0.5)).map(toChartData)
    }

    val series2 = new XYChart.Series[Number, Number] {
      name = "Series 2"
      data = Seq(
        (0.0, 1.6),
        (0.8, 0.4),
        (1.4, 2.9),
        (2.1, 1.3),
        (2.6, 0.9)).map(toChartData)
    }

    val chart = new LineChart[Number, Number](xAxis, yAxis, ObservableBuffer(series1, series2))
    chart.setTitle("頻譜圖")
    chart
  }

  override def getTitle: String = "操作"
}
