package com.apaa


import akka.actor
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.scaladsl.adapter._

object SoundCamClient {
  def apply(): Behavior[Command] =
    Behaviors.setup { context =>
      //#create-actors
      val system = actor.ActorSystem("ClassicSystem")
      val protocol = system.actorOf(SoundCamProtocol.props(context.self))
      //#create-actors

      Behaviors.receiveMessage {
        case DiscoverSoundCam =>
          context.log.info("FindSoundCam!")
          Behaviors.same
        case ConnectSoundCam(address, port) =>
          Behaviors.same

      }

    }

  sealed trait Command

  final case class ConnectSoundCam(address: String, port: Int) extends Command

  final case object DiscoverSoundCam extends Command
  final case object SoundCamConnected extends Command
  final case object SoundCamConnectFailed extends Command
  final case object ConnectionClosed extends Command
}
