package com.apaa


import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp, Udp}
import akka.util.ByteString
import com.apaa.SoundCamProtocolHelper.identificationRespHandler

import java.net.InetSocketAddress
import java.nio.{ByteBuffer, ByteOrder}

object SoundCamProtocol {
  private var invokeID: Byte = 0

  def createRequest(cmd: Byte, len: Int) = {
    invokeID = ((invokeID + 1) & 0xFF).toByte
    val byteBuffer = ByteBuffer.allocate(8 + len)
    byteBuffer.put(0, cmd)
    byteBuffer.put(1, invokeID)
    byteBuffer.put(2, 0)
    byteBuffer.put(3, 0)
    byteBuffer.putInt(4, len)
    byteBuffer
  }

  def props(client: akka.actor.typed.ActorRef[SoundCamClient.Command]) = Props(classOf[SoundCamProtocol], client)

  trait Command

  //Internal
  case class RequestHeader(cmd: Int, invokeID: Byte, len: Int)

  case class ResponseHeader(cmd: Int, invokeID: Byte, status: Int, len: Int)

  case class DataHeader(cmd: Int, invokeID: Byte, len: Int)

  final case class DataPacket(header: DataHeader, data: ByteString) extends Command

  final case class SoundCamParam(ip: String, port: Int) extends Command

  final case class ConnectSoundCam(ip: String, port: Int) extends Command

  final case object FindSoundCam extends Command

  final case object IDRequest extends Command

  final case object Reset extends Command


}

class SoundCamProtocol(client: akka.actor.typed.ActorRef[SoundCamClient.Command]) extends Actor with ActorLogging {

  import SoundCamClient._
  import SoundCamProtocol._
  import SoundCamProtocolHelper._

  log.info("SoundCamProtocol online")
  implicit val sys = context.system.classicSystem
  val tcpManager: ActorRef = IO(Tcp)
  val udpManager = IO(Udp)

  def discovery(udpSender: ActorRef): Receive = {
    case DiscoverSoundCam =>
      val addr = new InetSocketAddress("255.255.255.255", 51914)
      udpSender ! Udp.Send(ByteString("Hello AKAMs send your ID"), addr)

    case Udp.Received(data, remote) =>
      log.info(data.toString())

    case Udp.Unbind => udpSender ! Udp.Unbind
  }

  def unconnected(): Receive = {
    case Udp.Bound(local) =>
      log.info("Udp bound.")
      context become discovery(sender())
      self ! DiscoverSoundCam

    case DiscoverSoundCam =>
      //udpManager ! Udp.Bind(self, new InetSocketAddress("localhost", 51915))
      // context become(discovery())
      self ! ConnectSoundCam("192.168.1.107", 6340)

    case ConnectSoundCam(ip, port) =>
      val addr = new InetSocketAddress(ip, port)
      tcpManager ! Connect(addr)

    case Tcp.CommandFailed(_: Connect) =>
      client ! SoundCamConnectFailed

    case c@Connected(remoteAddress, localAddress) =>
      val connection = sender()
      connection ! Register(self)
      client ! SoundCamConnected
      context become connected(connection, ByteString.empty)
  }

  def replyResponse(cmd:Int, data: ByteBuffer, dataLen: Int)={
    import SoundCamProtocolHelper._
    if(handleMap.contains(cmd)){
      handleMap(cmd)(client, cmd, data, dataLen)
    }else
      log.error(s"Unknown cmd = $cmd")
  }

  def handleResponseFrame(frame: ByteString): Unit = {
    val buffer = frame.asByteBuffer
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val cmd = buffer.get(0)
    val invokeID = buffer.get(1)
    val status = buffer.getInt(4)
    val len = buffer.getInt(8)
    val header = ResponseHeader(cmd, invokeID, status, len)

    if(status != 0)
      client ! ErrorResponse(header, frame.drop(12))
    else{
      val data = frame.drop(12)
      assert(len == data.length)
      val dataBuffer = data.asByteBuffer
      dataBuffer.order(ByteOrder.LITTLE_ENDIAN)
      replyResponse(cmd&0xff, dataBuffer, data.length)
    }
  }

  def handleDataFrame(frame: ByteString): Unit = {
    val buffer = frame.asByteBuffer
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val cmd = buffer.get(0)
    val invokeID = buffer.get(1)
    val len = buffer.getInt(4)

    val header = DataHeader(cmd, invokeID, len)
    val data = frame.drop(8)
    self ! DataPacket(header, data)
  }

  def connected(connection: ActorRef, buffer: ByteString): Receive = {
    case IDRequest =>
      log.info("Send IDRequest...")
      val request = createRequest(IdentificationReq.toByte, 4)
      request.putInt(8, 2)
      connection ! Write(ByteString(request))

    case CommandFailed(w: Write) =>
      // O/S buffer was full
      log.error("OS buffer was full!")

    case Received(data) =>
      log.debug(s"Received ${data.length}")
      val newBuffer = buffer ++ data

      val cmd = newBuffer(0) & 0xff
      val (isDataFrame, headerLen, remain_offset)=
        if (cmd == DataMessage) { // DataPacket
          (true, 8, 4)
        } else {
          (false, 12, 8)
        }

      if (newBuffer.length >= headerLen) {
        val byteBuffer = newBuffer.asByteBuffer
        byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
        val remain = byteBuffer.getInt(remain_offset)
        val total = remain + headerLen
        log.debug(s"bufferLen=${newBuffer.length} remain=$remain total=$total")
        if (total <= newBuffer.length) {
          val frame = newBuffer.take(total)
          if(isDataFrame)
            handleDataFrame(frame)
          else
            handleResponseFrame(frame)

          context become connected(connection, newBuffer.drop(total))
        } else
          context become connected(connection, newBuffer)
      } else
        context become connected(connection, newBuffer)

    case DataPacket(header, data)=>

    case cc: ConnectionClosed =>
      log.info(cc.toString)
      client ! ConnectionClosed
      context become unconnected()
  }

  override def receive: Receive = unconnected()

}


