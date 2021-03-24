package com.apaa

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object SoundCamClient {
  def apply(): Behavior[Command] =
    Behaviors.setup { context =>
      //#create-actors
      //val greeter = context.spawn(Greeter(), "soundCamProtocol")
      //#create-actors

      Behaviors.receiveMessage {
        case FindSoundCam =>
          context.log.info("FindSoundCam!")
          Behaviors.same
        case ConnectSoundCam(address, port) =>
          Behaviors.same

      }

    }

  sealed trait Command

  sealed case class ConnectSoundCam(address: String, port: Int) extends Command

  case object FindSoundCam extends Command

}
