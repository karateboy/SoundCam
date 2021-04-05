package com.apaa.pages

import akka.actor.typed.ActorRef
import com.apaa.SoundCamClient._
import com.apaa.{DataObjectID, DeviceState, SoundCamApp, SoundCamClient}
import org.slf4j.LoggerFactory
import scalafx.scene.Node
import scalafx.scene.control.Button
import scalafx.scene.layout.{HBox, VBox}

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
          CameraResolution(320, 240),
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
    val vbox = new VBox(toolbar)
    vbox
  }

  override def getTitle: String = "操作"
}
