package com.apaa

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object SoundCamProtocol {
  trait Command
  case class ConnectSoundCam(ip:String, port:Int) extends  Command

  def apply(): Behavior[Command] = Behaviors.setup { context =>
    //#create-actors
    //val greeter = context.spawn(Greeter(), "soundCamProtocol")
    //#create-actors

    Behaviors.receiveMessage {
      case ConnectSoundCam(ip, port)=>
        Behaviors.same
    }
  }
}
