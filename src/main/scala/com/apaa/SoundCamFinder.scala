package com.apaa

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.io.{IO, Udp}
import com.apaa.SoundCamApp.soundCamApp.executionContext
import com.apaa.SoundCamClient.DiscoverSoundCam

import java.net.{InetSocketAddress, InterfaceAddress}
import scala.concurrent.duration.DurationInt
object SoundCamFinder {
  def props(client: ActorRef): Props = Props(classOf[SoundCamFinder], client)

}
class SoundCamFinder(client:ActorRef) extends Actor with ActorLogging {
  implicit val sys: ActorSystem = context.system.classicSystem
  //IO(Udp) ! Udp.Bind(self, new InetSocketAddress("localhost", 0))


  import java.net.{DatagramPacket, DatagramSocket, InetAddress}

  for(addr <- listLocalAddresses){
    IO(Udp) ! Udp.Bind(self, new InetSocketAddress(addr, 51915),
      Seq(Udp.SO.Broadcast(true)))
  }

  def broadcast(broadcastMessage: String, address: InetAddress, port:Int): Unit = {
    val socket = new DatagramSocket
    socket.setBroadcast(true)
    val buffer = broadcastMessage.getBytes
    val packet = new DatagramPacket(buffer, buffer.length, address, port)
    socket.send(packet)
    socket.close
  }

  import java.net.{InetAddress, NetworkInterface, SocketException}

  @throws[SocketException]
  def listBroadcastAddresses: Iterator[InetAddress] = {
    import scala.jdk.CollectionConverters._

    val interfaces = NetworkInterface.getNetworkInterfaces.asIterator().asScala
    val broadcastList: Iterator[Seq[InetAddress]] =
      for(networkInterface <- interfaces) yield {
        if(networkInterface.isLoopback() || !networkInterface.isUp())
          Seq.empty[InetAddress]
        else {
          val list: Seq[InterfaceAddress] = networkInterface.getInterfaceAddresses.asScala.toSeq
          list.map(_.getBroadcast).filter(a=> a!= null)
        }
      }
    broadcastList.flatten
  }

  def listLocalAddresses: Iterator[InetAddress] = {
    import scala.jdk.CollectionConverters._

    val interfaces = NetworkInterface.getNetworkInterfaces.asIterator().asScala
    val addressList: Iterator[Seq[InetAddress]] =
      for(networkInterface <- interfaces) yield {
        if(networkInterface.isLoopback() || !networkInterface.isUp())
          Seq.empty[InetAddress]
        else {
          val list: Seq[InterfaceAddress] = networkInterface.getInterfaceAddresses.asScala.toSeq
          list.map(_.getAddress).filter(a=> a!= null)
        }
      }
    addressList.flatten
  }

  def bound(socketList: Seq[ActorRef]): Receive = {
    case DiscoverSoundCam =>
      for(inetaddr <- listBroadcastAddresses){
        log.info(s"send to $inetaddr")
        broadcast("Hello AKAMs send your ID", inetaddr, 51914)
      }

    case Udp.Received(data, remote) =>
      val buffer = data.asByteBuffer
      val bufferLen = data.length
      //val len = buffer.getInt(8)
      val ip = buffer.getInt(bufferLen - 8)
      val ipstr = String.format("%d.%d.%d.%d", (ip & 0xff), (ip >> 8 & 0xff), (ip >> 16 & 0xff), (ip >> 24 & 0xff))
      log.info(ipstr)

      client ! com.apaa.SoundCamProtocol.ClientIP(ipstr)

    case Udp.Unbind =>
      for(socket <- socketList)
        socket ! Udp.Unbind

    case Udp.Bound(local) =>
      log.info(s"Udp bound. $local ${sender()}")
      context become bound(socketList.+:(sender()))
  }

  override def receive: Receive = {
    case Udp.Bound(local) =>
      log.info(s"Udp bound. $local ${sender()}")
      context become bound(Seq(sender()))

    case req@DiscoverSoundCam =>
      log.debug(s"reissue $req")
      context.system.scheduler.scheduleOnce(1.seconds, self, req)
  }
}
