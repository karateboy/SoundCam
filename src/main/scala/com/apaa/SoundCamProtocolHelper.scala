package com.apaa

import com.apaa.SoundCamClient._
import org.slf4j.LoggerFactory

import java.nio.{ByteBuffer, ByteOrder}

object SoundCamProtocolHelper {
  type ResponseHandler = (akka.actor.typed.ActorRef[SoundCamClient.Command], Int, ByteBuffer) => Unit
  val logger = LoggerFactory.getLogger(this.getClass)
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
    ReadDataObjectRes -> readDataObjectRespHandler,
    DataMessage -> readDataObjectRespHandler
  )

  def identificationRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
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

  def resetRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    assert(cmd == ResetRes)
    client ! ResetResponse
  }

  def prepareStateRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    assert(cmd == PrepareStateRes)
    client ! PrepareStateResponse
  }

  def finishStateRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    assert(cmd == FinishStateRes)
    client ! FinishStateResponse
  }

  def startProcedureRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    assert(cmd == StartProcedureRes)
    client ! StartProcedureResponse
  }

  def stopProcedureRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    assert(cmd == StopProcedureRes)
    client ! StopProcedureResponse
  }

  def writeDataObjectRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    assert(cmd == WriteDataObjectRes)
    client ! WriteDataObjectResponse
  }

  def readDataObjectRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer) = {
    val count = data.getInt
    var remainByteBuffer = data.slice(4, data.remaining())
    remainByteBuffer.order(ByteOrder.LITTLE_ENDIAN)

    def handleDataObject(data: ByteBuffer) = {
      val objectID = data.getShort(0)
      val headerLen = 8

      logger.debug(s"objectID = $objectID")

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

      val objectLen: Int =
        objectID match {
          case DataObjectID.CommonStatus =>
            val followingLen = data.getInt(4)
            val errorCode = data.getInt(8)
            val deviceState = data.getInt(12)
            val subState = data.getInt(16)
            client ! CommonStatus(errorCode, deviceState, subState)
            headerLen + followingLen
          case DataObjectID.Distance =>
            val distance = data.getInt(4)
            client ! Distance(distance)
            8
          case DataObjectID.FrequencyRange =>
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
          case DataObjectID.CameraLighting =>
            val on = data.getInt(4)
            client ! CameraLight(on != 0)
            8
          case DataObjectID.MicrophonePosition =>
            val len = data.getInt(4)
            val count = data.getInt(8)
            12 + 24 * count
          case DataObjectID.MicrophoneWeighting =>
            val len = data.getInt(4)
            val count = data.getInt(8)
            assert(len + 4 == 12 + 8 * count)
            12 + 8 * count
          case DataObjectID.Alignment =>
            68
          case DataObjectID.VideoData =>
            val len = data.getInt(4)
            val timestamp = data.getLong(8)
            val h = data.getShort(16)
            val v = data.getShort(18)
            val video = data.slice(20, len - 12)
            client ! VideoData(timestamp, h, v, video)
            headerLen + len
          case DataObjectID.AcousticVideoData =>
            val len = data.getInt(4)
            val timestamp = data.getLong(8)
            val freqMin = data.getShort(16)
            val freqMax = data.getShort(18)
            val distance = data.getInt(20)
            val acousticData = data.slice(24, 3072 * 4).order(ByteOrder.LITTLE_ENDIAN)
              .asFloatBuffer()
            logger.info(s"acoustic remaining=${acousticData.remaining()} isDirect=${acousticData.isDirect}")
            val floatArray = new Array[Float](3072)
            acousticData.get(floatArray)
            client ! AcousticImage(timestamp, freqMin, freqMax, distance, floatArray)
            headerLen + len
          case DataObjectID.AudioData =>
            val len = data.getInt(4)
            val timestamp = data.getLong(8)
            val h = data.getShort(16)
            val v = data.getShort(18)
            val dp = data.slice(20, 8192).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
            val dm = data.slice(20+8192, 8192).order(ByteOrder.LITTLE_ENDIAN).asIntBuffer()
            val dataPlus = new Array[Int](2048)
            dp.get(dataPlus)
            val dataMinus = new Array[Int](2048)
            dm.get(dataMinus)
            val dt = data.getDouble(20+2*8192)
            val filtered = data.getShort(20+2*8192 + 8) != 0
            val freqMin = data.getInt(20+2*8192 + 8+2)
            val freqMax = data.getInt(20+2*8192 + 8+2 + 4)
            client ! AudioData(timestamp, h, v, dataPlus, dataMinus, dt, filtered, freqMin, freqMax)
            headerLen + len
          case DataObjectID.SpectrumData =>
            val len = data.getInt(4)
            val timestamp = data.getLong(8)
            val df = data.getFloat(16)
            val filtered = data.getShort(20) != 0
            val freqMin = data.getInt(22)
            val freqMax = data.getInt(26)
            val sg = data.slice(30, 1024*4).order(ByteOrder.LITTLE_ENDIAN).asFloatBuffer()
            val sl = data.slice(30 + 1024*4, 1024*4).order(ByteOrder.LITTLE_ENDIAN).asFloatBuffer()
            val globalSpectrum = new Array[Float](1024)
            sg.get(globalSpectrum)
            val localSpectrum = new Array[Float](1024)
            sl.get(localSpectrum)
            client ! Spectrum(timestamp, delta = df, filtered, freqMin, freqMax, globalSpectrum , localSpectrum)
            headerLen + len
          case DataObjectID.CameraStatus =>
            val followingLen = data.getInt(4)
            val dateTime = getDateTime(8)
            val temp = data.getFloat(16)
            client ! CameraStatus(dateTime = dateTime, temperature = temp)
            headerLen + followingLen
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
              localSoundFiltered = lsf, levelFiltered = lf, rawDataFiltered = rdf)
            16
          case x =>
            val followingLen = data.getInt(4)
            logger.error(s"unexpected $x data object following $followingLen")
            headerLen + followingLen
        }
      objectLen
    }

    logger.debug(s"count=$count")
    for (idx <- 1 to count) {
      val dataObjectLen = handleDataObject(remainByteBuffer)
      if (idx < count) {
        remainByteBuffer = remainByteBuffer.slice(dataObjectLen, remainByteBuffer.remaining())
        remainByteBuffer.order(ByteOrder.LITTLE_ENDIAN)
      }
    }
  }
}
