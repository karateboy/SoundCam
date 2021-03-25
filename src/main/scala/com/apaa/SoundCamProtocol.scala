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
  final case class SoundCamParam(ip:String, port:Int) extends Command

  case class ConnectSoundCam(ip:String, port:Int) extends  Command


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
