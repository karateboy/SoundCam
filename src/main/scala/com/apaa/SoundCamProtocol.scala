package com.apaa

import akka.actor.{ActorRef, ActorSystem, typed}
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.io.IO
import akka.stream.scaladsl._

import java.net.InetSocketAddress


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


  def apply(): Behavior[Command] = Behaviors.setup { context =>

    implicit val sys = context.system.classicSystem
    //#create-actors
    //val greeter = context.spawn(Greeter(), "soundCamProtocol")
    //#create-actors
    val addr = new InetSocketAddress("127.0.0.1", 123)
    val manager: ActorRef = IO(Tcp)

    Behaviors.receiveMessage {
      case ConnectSoundCam(ip, port)=>
        Behaviors.same
    }
  }

  def unconnected():Behavior[Command] = Behaviors.setup{
    context =>
      Behaviors.receiveMessage{
        case FindSoundCam =>
          Behaviors.ignore
      }
  }

  def connected():Behavior[Command] = Behaviors.receiveMessage{
    case ConnectSoundCam(ip, port)=>
      Behaviors.ignore
  }
}

class SoundCamProtocol private (){

}
