package com.apaa

import akka.event.LoggingAdapter
import akka.util.ByteString
import com.apaa.SoundCamClient._

import java.nio.ByteBuffer

object SoundCamProtocolHelper {

  type ResponseHandler = (akka.actor.typed.ActorRef[SoundCamClient.Command], Int, ByteBuffer, LoggingAdapter) => Unit
  val ResetReq = 0x80
  val ResetRes = 0x0
  val IdentificationReq = 0x84
  val IdentificationRes = 0x4
  val PrepareStateReq = 0x86
  val PrepareStateRes = 0x6
  val FinishStateReq = 0x87
  val FinishStateRes = 0x7
  val StopProcedureReq = 0x89
  val StopProcedureRes = 0x9
  val StartProcedureReq = 0x8A
  val StartProcedureRes = 0xA
  val ReadDataObjectReq = 0x92
  val ReadDataObjectRes = 0x12
  val WriteDataObjectReq = 0x93
  val WriteDataObjectRes = 0x13
  val DataMessage = 0x41
  val handleMap: Map[Int, ResponseHandler] = Map(
    IdentificationRes -> identificationRespHandler,
    ResetRes -> resetRespHandler,
    PrepareStateRes -> prepareStateRespHandler,
    FinishStateRes -> finishStateRespHandler,
    StartProcedureRes -> startProcedureRespHandler,
    StopProcedureRes -> stopProcedureRespHandler,
    WriteDataObjectRes -> writeDataObjectRespHandler,
    ReadDataObjectRes -> readDataObjectRespHandler
  )

  def identificationRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == IdentificationRes)
    val protocol = data.getInt(0)
    val deviceId = data.getInt(4)
    val deviceMode = data.getInt(8)
    val deviceError = data.getInt(12)
    val serial = data.getInt(16)
    //siliconSerial 20
    //HardwareVersion 28
    //BootloaderVersion 32
    //FirmwareVersion 36
    //FPGAVersion 40
    //ProductionDate 44
    //CalibrationDate 52
    //MacAddr 60
    //IPAddr 68
    //PCBSerial 72

    client ! IdentificationResponse(
      protocol = protocol, deviceId = deviceId, deviceMode = deviceMode,
      deviceError = deviceError, serial = serial)
  }

  def resetRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == ResetRes)
    client ! ResetResponse
  }

  def prepareStateRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == PrepareStateRes)
    client ! PrepareStateResponse
  }

  def finishStateRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == FinishStateRes)
    client ! FinishStateResponse
  }

  def startProcedureRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == StartProcedureRes)
    client ! StartProcedureResponse
  }

  def stopProcedureRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == StopProcedureRes)
    client ! StopProcedureResponse
  }

  def writeDataObjectRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == WriteDataObjectRes)
    client ! WriteDataObjectResponse
  }

  def readDataObjectRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, log: LoggingAdapter) = {
    assert(cmd == ReadDataObjectRes)
    val count = data.getInt(0)
    var start = 4

    def getDateTime(offset: Int) = {
      val year = data.getShort(offset)
      val month = data.get(offset + 2)
      val day = data.get(offset + 3)
      val hour = data.get(offset + 4)
      val min = data.get(offset + 5)
      val sec = data.get(offset + 6)
      val kind = data.get(offset + 7)
      CurrentDateTime(year, month, day, hour, min, sec, kind == 0)
    }

    def handleDataObject(start: Int) = {
      val objectID = data.getShort(start)
      val headerLen = 8

      val objectLen: Int =
        objectID match {
          case DataObjectID.CommonStatus =>
            val followingLen = data.getInt(start + 4)
            val errorCode = data.getInt(start + 8)
            val deviceState = data.getInt(start + 12)
            val subState = data.getInt(start + 16)
            client ! CommonStatus(errorCode, deviceState, subState)
            headerLen + followingLen
          case DataObjectID.CameraStatus =>
            val followingLen = data.getInt(start + 4)
            val dateTime = getDateTime(start + 8)
            val temp = data.getFloat(start + 16)
            client ! CameraStatus(dateTime = dateTime, temperature = temp)
            headerLen + followingLen
          case DataObjectID.Distance =>
            val distance = data.getInt(4)
            client ! Distance(distance)
            headerLen
          case DataObjectID.FrequencyRange=>
            val min = data.getInt(4)
            val max = data.getInt(8)
            client ! FrequencyRange(min, max)
            12
          case DataObjectID.CameraResolution =>
            val h = data.getShort(4)
            val v = data.getShort(6)
            client ! CameraResolution(h, v)
            8
          case DataObjectID.CameraBrightness =>
            val b = data.getInt(4)
            client ! CameraBrightness(b)
            8
          case DataObjectID.AcousticFrameRate =>
            val rate = data.getInt(4)
            client ! AcousticFrameRate(rate)
            8
          case DataObjectID.VideoFrameRate =>
            val rate = data.getInt(4)
            client ! VideoFrameRate(rate)
            8
          case DataObjectID.LocalSoundTargetCoordinate =>
            val h = data.getShort(4)
            val v = data.getShort(6)
            client ! LocalSoundTargetCoordinates(h, v)
            8
          case DataObjectID.DataToSend =>
            val ap = data.get(4) != 0
            val vp = data.get(5) != 0
            val ls = data.get(6) != 0
            val ml = data.get(7) != 0
            val mrd = data.get(8) != 0
            val sp = data.get(9) != 0
            val smd = data.get(10) != 0
            val lsf = data.get(11) != 0
            val lf = data.get(12) != 0
            val rdf = data.get(13) != 0
            client ! DataToSend(acousticPicture = ap, videoPictures = vp, localSound = ls,
              micLevel = ml, micRawData = mrd, spectra = sp, singleMicData = smd,
              localSoundFiltered = lsf, levelFiltered= lf, rawDataFiltered = rdf)
            16
          case x =>
            val followingLen = data.getInt(start + 4)
            log.error(s"unexpected $x data object following $followingLen")
            headerLen + followingLen
        }
      objectLen
    }

    log.info(s"count=$count")
    for (idx <- 1 to count) {
      log.info(s"$idx start=$start")
      start = start + handleDataObject(start)
    }

  }
}
