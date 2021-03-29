package com.apaa


import akka.actor
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors
import akka.util.ByteString
import com.apaa.SoundCamProtocol.{IDRequest, ResponseHeader}

object SoundCamClient {
  def apply(): Behavior[Command] =
    Behaviors.setup { context =>
      //#create-actors
      val system = actor.ActorSystem("ClassicSystem")
      val protocol = system.actorOf(SoundCamProtocol.props(context.self))
      //#create-actors

      protocol ! DiscoverSoundCam

      Behaviors.receiveMessage {
        case DiscoverSoundCam =>
          context.log.info("DiscoverSoundCam!")
          protocol ! DiscoverSoundCam
          Behaviors.same

        case ConnectionClosed =>
          context.log.info("ConnectionClosed")
          Behaviors.same

        case SoundCamConnectFailed =>
          context.log.error("SoundCamConnectFailed")
          Behaviors.same

        case SoundCamConnected =>
          context.log.info("SoundCamConnected")
          protocol ! IDRequest
          Behaviors.same

        case response: IdentificationResponse =>
          context.log.info(s"serial=${response.serial}")
          Behaviors.same

        case PrepareStateResponse =>
          Behaviors.same

        case StartProcedureResponse =>
          Behaviors.same

        case StopProcedureResponse =>
          Behaviors.same

        case FinishStateResponse=>
          Behaviors.same

        case ResetResponse =>
          Behaviors.same

        case WriteDataObjectResponse=>
          Behaviors.same

        case dt:CurrentDateTime =>
          Behaviors.same
        case errorResponse: ErrorResponse =>
          Behaviors.same
      }

    }

  sealed trait Command

  final case class ErrorResponse(header: ResponseHeader, data: ByteString) extends Command

  final case class IdentificationResponse(protocol: Int, deviceId: Int, deviceMode: Int,
                                          deviceError: Int, serial: Int) extends Command

  final case object DiscoverSoundCam extends Command

  final case object SoundCamConnected extends Command

  final case object SoundCamConnectFailed extends Command

  final case object ConnectionClosed extends Command

  final case object ResetResponse extends Command

  final case object PrepareStateResponse extends Command

  final case object FinishStateResponse extends Command

  final case object StartProcedureResponse extends Command

  final case object StopProcedureResponse extends Command

  final case object WriteDataObjectResponse extends Command

  //final case class CommonStatus(deviceState:Int, subState:Int) extends Command
  //final case class Distance(value:Int) extends Command
  //final case class FrequencyRange(min:Int, max:Int) extends Command
  final case class CurrentDateTime(year:Int, month:Int, day:Int, hour:Int, min:Int, sec:Int) extends Command
}
