package com.apaa


import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.io.{IO, Tcp, Udp}
import akka.io.Tcp._
import akka.util.ByteString
import com.apaa.SoundCamClient.DiscoverSoundCam

object SoundCamProtocol {
  trait Command
  final case object FindSoundCam extends  Command
  case class RequestHeader(cmd:Byte, invokeID:Byte, reserved:Short, len:Int)
  case class ResponseHeader(cmd:Byte, invokeID:Byte, reserved:Short, status:Int, len:Int)
  case class DataHeader(cmd:Byte, invokeID:Byte, reserved:Short, len:Int)

  final case class SoundCamParam(ip:String, port:Int) extends Command
  case class ConnectSoundCam(ip:String, port:Int) extends  Command

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
  val ReadDataObjectReq = 0x92
  val ReadDataObjectRes = 0x12
  val WriteDataObjectReq = 0x93
  val WriteDataObjectRes = 0x13
  val DataMessage = 0x41

  case class ConnectReq(protocolVersion:Int) extends Command
  case class ConnectResp(protocolVersion:Int,
                         deviceID:Int) extends Command
  case object Reset extends Command


  def props(client: akka.actor.typed.ActorRef[SoundCamClient.Command]) = Props(classOf[SoundCamProtocol], client)


}

class SoundCamProtocol(client: akka.actor.typed.ActorRef[SoundCamClient.Command]) extends Actor with ActorLogging
{
  import SoundCamProtocol._
  import SoundCamClient._

  log.info("SoundCamProtocol online")
  implicit val sys = context.system.classicSystem
  val tcpManager: ActorRef = IO(Tcp)
  val udpManager = IO(Udp)

  def unconnected() : Receive = {
    case Tcp.CommandFailed(_: Connect) =>
       client ! SoundCamConnectFailed

    case c @ Connected(remoteAddress, localAddress) =>
      val connection = sender()
      connection ! Register(self)
      client ! SoundCamConnected
      context become connected(connection)
  }

  def connected(connection: ActorRef):Receive = {
    case data: ByteString =>
      connection ! Write(data)
    case CommandFailed(w: Write) =>
      // O/S buffer was full
      log.error("OS buffer was full!")

    case Received(data) =>

    case _:ConnectionClosed =>
      client ! ConnectionClosed
      context become unconnected()
  }

  override def receive: Receive = unconnected()

  override def postStop(): Unit = {
    super.postStop()
  }
}


