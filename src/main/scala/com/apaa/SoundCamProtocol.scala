package com.apaa


import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import com.apaa.SoundCamClient._
import com.apaa.SoundCamProtocolHelper.WriteDataObjectReq
import org.slf4j.LoggerFactory

import java.net.InetSocketAddress
import java.nio.{ByteBuffer, ByteOrder}

object DeviceState {
  val Idle = 0
  val Measuring = 1
  val Service = 3
  val Ultrasonic = 4
}

object DeviceSubState {
  val Idle = 0
  val Running = 1
  val Preparing = 16
  val Finished = 128
  val Aborted = 129
}

object SoundCamProtocol {
  val logger = LoggerFactory.getLogger(this.getClass)
  private var invokeID: Byte = 0

  def createRequest(cmd: Byte, len: Int) = {
    invokeID = ((invokeID + 1) & 0xFF).toByte
    val byteBuffer = ByteBuffer.allocate(8 + len)
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    byteBuffer.put(0, cmd)
    byteBuffer.put(1, invokeID)
    byteBuffer.putShort(2, 0)
    byteBuffer.putInt(4, len)
    byteBuffer
  }

  def writeDataObjectRequest(dataObjects: Seq[DataObject]) = {
    def dataObjectToByteString(dataObject: DataObject) = {
      dataObject match {
        case data: Distance =>
          val buffer = ByteBuffer.allocate(8)
          buffer.order(ByteOrder.LITTLE_ENDIAN)
          buffer.putShort(0, 2)
          buffer.putShort(2, 7)
          buffer.putInt(4, data.value)
          ByteString(buffer)
        case data: FrequencyRange =>
          val buffer = ByteBuffer.allocate(8)
          buffer.order(ByteOrder.LITTLE_ENDIAN)
          buffer.putShort(0, 3)
          buffer.putShort(2, 0x100)
          buffer.putShort(4, data.min.toShort)
          buffer.putShort(6, data.max.toShort)
          ByteString(buffer)
        case data: CameraResolution =>
          val buffer = ByteBuffer.allocate(8)
          buffer.order(ByteOrder.LITTLE_ENDIAN)
          buffer.putShort(0, 4)
          buffer.putShort(2, 0x200)
          buffer.putShort(4, data.h)
          buffer.putShort(6, data.v)
          ByteString(buffer)
        case data: VideoFrameRate =>
          val buffer = ByteBuffer.allocate(8)
          buffer.order(ByteOrder.LITTLE_ENDIAN)
          buffer.putShort(0, 7)
          buffer.putShort(2, 7)
          buffer.putInt(4, data.rate)
          ByteString(buffer)
        case data: AcousticFrameRate =>
          val buffer = ByteBuffer.allocate(8)
          buffer.order(ByteOrder.LITTLE_ENDIAN)
          buffer.putShort(0, 6)
          buffer.putShort(2, 7)
          buffer.putInt(4, data.rate)
          ByteString(buffer)
        case data: CurrentDateTime =>
          val buffer = ByteBuffer.allocate(16)
          buffer.order(ByteOrder.LITTLE_ENDIAN)
          buffer.putShort(0, 0xC000.toShort)
          buffer.putShort(2, 0xE)
          buffer.putInt(4, 8)
          buffer.putShort(8, data.year.toShort)
          buffer.put(10, data.month.toByte)
          buffer.put(11, data.day.toByte)
          buffer.put(12, data.hour.toByte)
          buffer.put(13, data.min.toByte)
          buffer.put(14, data.sec.toByte)
          val kind = if(data.utc) 0 else 1
          buffer.put(15, kind.toByte)
          ByteString(buffer)
        case data =>
          logger.error(s"unhandled ${data.toString}")
          ByteString.empty
      }
    }

    val byteBuffer = ByteBuffer.allocate(12)
    byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    byteBuffer.put(0, WriteDataObjectReq.toByte)
    byteBuffer.put(1, invokeID)
    byteBuffer.putShort(2, 0)

    val byteStringOfDataObjects: Seq[ByteString] =
      for (dataObject <- dataObjects) yield
        dataObjectToByteString(dataObject)

    val lens = byteStringOfDataObjects map {
      _.length
    }
    val total = lens.foldLeft(0)((a, b) => a + b)

    // Data length
    byteBuffer.putInt(4, 4 + total)
    byteBuffer.putInt(8, dataObjects.length)

    val cmd = ByteString(byteBuffer)
    byteStringOfDataObjects.foldLeft(cmd)((a, b)=> a ++ b)
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

  final case class ClientIP(addr:String) extends Command
}

class SoundCamProtocol(client: akka.actor.typed.ActorRef[SoundCamClient.Command]) extends Actor with ActorLogging {

  import SoundCamProtocol._
  import SoundCamProtocolHelper._

  log.info("SoundCamProtocol online")
  implicit val sys = context.system.classicSystem
  val tcpManager: ActorRef = IO(Tcp)

  val finder = context.actorOf(SoundCamFinder.props(self), "finder")

  def unconnected(): Receive = {
    case DiscoverSoundCam =>
      //udpManager ! Udp.Bind(self, new InetSocketAddress("localhost", 51915))
      // context become(discovery())
      finder ! DiscoverSoundCam
      //self ! ConnectSoundCam(client_ip, 6340)

    case ClientIP(addr) =>
      self ! ConnectSoundCam(addr, 6340)

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

  def replyResponse(cmd: Int, data: ByteString) = {
    import SoundCamProtocolHelper._
    if (handleMap.contains(cmd)) {
      handleMap(cmd)(client, cmd, data.asByteBuffer.order(ByteOrder.LITTLE_ENDIAN))
    } else
      log.error(s"Unknown cmd = $cmd")
  }

  def handleResponseFrame(frame: ByteString): Unit = {
    val buffer = frame.asByteBuffer.order(ByteOrder.LITTLE_ENDIAN)
    val cmd = buffer.get(0)
    val invokeID = buffer.get(1)
    val status = buffer.getInt(4)
    val len = buffer.getInt(8)
    val header = ResponseHeader(cmd, invokeID, status, len)
    if (header.status != 0)
      client ! ErrorResponse(header, frame.drop(12))
    else {
      val data = frame.drop(12)
      replyResponse(cmd & 0xff, data)
    }
  }

  def handleDataFrame(frame: ByteString): Unit = {
    val buffer = frame.asByteBuffer
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    val cmd = buffer.get(0)
    //val invokeID = buffer.get(1)
    //val len = buffer.getInt(4)

    val data = frame.drop(8)
    replyResponse(cmd & 0xff, data)
  }

  def connected(connection: ActorRef, buffer: ByteString): Receive = {
    case IDRequest =>
      log.info("Send IDRequest...")
      val request = createRequest(IdentificationReq.toByte, 4)
      request.putInt(8, 2)
      connection ! Write(ByteString(request))

    case PrepareState(state) =>
      log.debug(s"Send PrepareState...${state}")
      val req = createRequest(PrepareStateReq.toByte, 4)
      req.putInt(8, state)
      connection ! Write(ByteString(req))

    case FinishState =>
      log.debug("Send FinishState")
      val req = createRequest(FinishStateReq.toByte, 0)
      connection ! Write(ByteString(req))

    case StartProcedure =>
      log.info("StartProcedure")
      val req = createRequest(StartProcedureReq.toByte, 0)
      connection ! Write(ByteString(req))

    case StopProcedure =>
      log.info("StopProcedure")
      val req = createRequest(StopProcedureReq.toByte, 0)
      connection ! Write(ByteString(req))

    case ReadDataObject(objectIDs) =>
      val req = createRequest(ReadDataObjectReq.toByte, 4 + objectIDs.length * 2)
      req.putInt(8, objectIDs.length)
      for ((id, idx) <- objectIDs.zipWithIndex) {
        req.putShort(12 + idx * 2, id.toShort)
      }
      connection ! Write(ByteString(req))
    case WriteDataObject(dataObjects) =>
      val req = writeDataObjectRequest(dataObjects)
      connection ! Write(req)

    case CommandFailed(w: Write) =>
      // O/S buffer was full
      log.error("OS buffer was full!")

    case Received(data) =>
      val newBuffer = buffer ++ data

      val cmd = newBuffer(0) & 0xff
      val (isDataFrame, headerLen, remain_offset) =
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
        if(isDataFrame)
          log.debug(s"bufferLen=${newBuffer.length} total=$total")
        if (total <= newBuffer.length) {
          val frame = newBuffer.take(total)
          if (isDataFrame)
            handleDataFrame(frame)
          else
            handleResponseFrame(frame)

          context become connected(connection, newBuffer.drop(total))
        } else
          context become connected(connection, newBuffer)
      } else
        context become connected(connection, newBuffer)

    case cc: ConnectionClosed =>
      log.info(cc.toString)
      client ! SoundCamConnectFailed
      context become unconnected()
  }

  override def receive: Receive = unconnected()

}


