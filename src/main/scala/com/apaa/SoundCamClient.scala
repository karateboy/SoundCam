package com.apaa

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object SoundCamClient {
  def apply(): Behavior[Command] =
    Behaviors.setup { context =>
      //#create-actors
      val protocol = context.spawn(SoundCamProtocol(), "soundCamProtocol")
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

  final case class ConnectSoundCam(address: String, port: Int) extends Command

  final case object FindSoundCam extends Command

}
