package com.apaa.pages

import akka.actor.typed.ActorRef
import com.apaa.SoundCamClient._
import com.apaa.{DeviceState, SoundCamApp, SoundCamClient, SoundCamInfoHandler}
import org.slf4j.LoggerFactory
import scalafx.collections.ObservableBuffer
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.Node
import scalafx.scene.chart.{BarChart, CategoryAxis, NumberAxis, XYChart}
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
    val spectrumChart: BarChart[String, Number] = getSpectrumChart
    SoundCamInfoHandler.setSpectrumChart(spectrumChart)
    grid.add(spectrumChart, 1, 1)

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

  def getDbaPane: GridPane = {
    val grid = new GridPane()
    grid.setAlignment(Pos.Center)
    grid.setHgap(10)
    grid.setVgap(10)
    grid.add(new Label("Global dB(A)"), 0, 0)
    val globalDBa = new TextField()
    globalDBa.setPrefWidth(60)
    globalDBa.setEditable(false)
    grid.add(globalDBa, 1, 0)

    grid.add(new Label("Local dB(A)"), 0, 1)
    val localDBa = new TextField()
    localDBa.setPrefWidth(60)
    localDBa.setEditable(false)
    SoundCamInfoHandler.setDbaTextField(globalDBa, localDBa)
    grid.add(localDBa, 1, 1)
    grid.add(new Label("Duo dB(A)"), 0, 2)
    val duoDBa = new TextField()
    duoDBa.setPrefWidth(60)
    duoDBa.setEditable(false)
    grid.add(duoDBa, 1, 2)
    SoundCamInfoHandler.setDuoDbA(duoDBa)
    grid.setPadding(Insets(10))
    grid
  }

  val ONE_THIRD_OCTAVE_BANDS_CENTER_FREQ: Seq[String] = Seq("6.3", "8", "10", "12.5", "16", "20", "25", "31.5",
    "40", "50", "63", "80", "100", "125", "160", "200", "250",
    "315", "400", "500", "630", "800", "1k", "1.25k", "1.6k",
    "2k", "2.5k", "3.15k", "4k", "5k", "6.3k", "8k", "10k", "12.5k", "16k", "20k")

  def getSpectrumChart: BarChart[String, Number] = {
    val xAxis = CategoryAxis("頻率 Hz")
    val yAxis = NumberAxis("聲壓 dB(A)")

    val toChartData = (xy: (String, Double)) => XYChart.Data[String, Number](xy._1, xy._2)

    val oneThirdData: Seq[(String, Double)] = for(name<-ONE_THIRD_OCTAVE_BANDS_CENTER_FREQ) yield
      (name, 100.0)
    val series1 = new XYChart.Series[String, Number] {
      name = "Series 1"
      data = oneThirdData.map(toChartData)
    }
    series1.setName("1/3 頻譜")

    val chart1 = new BarChart[String, Number](xAxis, yAxis, ObservableBuffer(series1))
    chart1.setTitle("1/3 頻譜圖")
    chart1
  }

  override def getTitle: String = "操作"
}
