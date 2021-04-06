package com.apaa.pages

import akka.actor.typed.ActorRef
import com.apaa.SoundCamClient._
import com.apaa.{DeviceState, SoundCamApp, SoundCamClient, SoundCamInfoHandler}
import org.slf4j.LoggerFactory
import scalafx.geometry.Insets
import scalafx.scene.Node
import scalafx.scene.control.Button
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout.{BorderPane, HBox}

import java.time.LocalDateTime

class OperationPage extends ContentPage {
  val logger = LoggerFactory.getLogger(this.getClass)

  override def getContent: Node = {
    val client: ActorRef[SoundCamClient.Command] = SoundCamApp.soundCamClient
    val btnPrepare = new Button("Prepare") {
      onAction = (_) => {
        // client !SoundCamClient.StartProcedure
        val now = LocalDateTime.now()
        val currentDateTime = CurrentDateTime(now.getYear, now.getMonthValue, now.getDayOfMonth,
          now.getHour, now.getMinute, now.getSecond, false)
        val dataObjects = Seq(
          Distance(100),
          FrequencyRange(100, 24000),
          CameraResolution(640, 480),
          VideoFrameRate(30),
          AcousticFrameRate(30),
          currentDateTime
        )
        client ! SoundCamClient.WriteDataObject(dataObjects)
        client ! SoundCamClient.PrepareState(DeviceState.Measuring)
        //client ! ReadDataObject(Seq(DataObjectID.DataToSend))
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
    val borderPane = new BorderPane()
    borderPane.setTop(toolbar)
    borderPane.setCenter(imageView)
    borderPane
  }

  override def getTitle: String = "操作"
}
