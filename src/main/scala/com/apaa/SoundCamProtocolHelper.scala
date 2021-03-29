package com.apaa

import com.apaa.SoundCamClient._

import java.nio.ByteBuffer

object SoundCamProtocolHelper {
  type ResponseHandler = (akka.actor.typed.ActorRef[SoundCamClient.Command], Int, ByteBuffer, Int) => Unit
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

  def identificationRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
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

  def resetRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == ResetRes)
    client ! ResetResponse
  }

  def prepareStateRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == PrepareStateRes)
    client ! PrepareStateResponse
  }

  def finishStateRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == FinishStateRes)
    client ! FinishStateResponse
  }

  def startProcedureRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == StartProcedureRes)
    client ! StartProcedureResponse
  }

  def stopProcedureRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == StopProcedureRes)
    client ! StopProcedureResponse
  }

  def writeDataObjectRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == WriteDataObjectRes)
    client ! WriteDataObjectResponse
  }
  def readDataObjectRespHandler(client: akka.actor.typed.ActorRef[SoundCamClient.Command], cmd: Int, data: ByteBuffer, dataLen: Int) = {
    assert(cmd == ReadDataObjectRes)

  }
}
