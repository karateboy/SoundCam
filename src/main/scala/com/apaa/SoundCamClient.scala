package com.apaa


import akka.actor
import akka.actor.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.ByteString
import com.apaa.SoundCamProtocol.{FinishState, IDRequest, PrepareState, ResponseHeader, StartProcedure, StopProcedure}

import scala.concurrent.duration.DurationInt

object SoundCamClient {
  //#create-actors
  val system = actor.ActorSystem("ClassicSystem")
  //#create-actors

  def apply(): Behavior[Command] = unconnected(None)

  def unconnected(protocolOpt: Option[ActorRef]): Behavior[Command] = Behaviors.setup { context =>

    val protocol = protocolOpt.getOrElse({
      system.actorOf(SoundCamProtocol.props(context.self), "SoundCamProtocol")
    })

    protocol ! DiscoverSoundCam

    Behaviors.receiveMessagePartial {
      case DiscoverSoundCam =>
        context.log.info("DiscoverSoundCam!")
        protocol ! DiscoverSoundCam
        Behaviors.same

      case SoundCamConnectFailed =>
        context.log.error("SoundCamConnectFailed")
        Behaviors.same

      case SoundCamConnected =>
        context.log.info("SoundCamConnected")
        protocol ! IDRequest
        connected(protocol, context)

      case _ =>
        Behaviors.unhandled
    }
  }

  def connected(protocol: ActorRef, context: ActorContext[Command]): Behavior[Command] = {
    Behaviors.withTimers({
      timers =>
        // instead of FSM state timeout
        //timers.startTimerWithFixedDelay(DiscoverSoundCam, 9.seconds)

        Behaviors.receiveMessagePartial({

          case ConnectionClosed =>
            context.log.info("ConnectionClosed")
            unconnected(Some(protocol))

          case response: IdentificationResponse =>
            context.log.debug(s"serial=${response.serial}")
            protocol ! StopProcedure
            Behaviors.same

          case r@PrepareStateResponse =>
            context.log.debug(r.toString)
            protocol ! FinishState
            Behaviors.same

          case r@StartProcedureResponse =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@StopProcedureResponse =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@FinishStateResponse =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@ResetResponse =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@WriteDataObjectResponse =>
            context.log.debug(r.toString)
            Behaviors.same

          case dt: CurrentDateTime =>
            Behaviors.same

          case ErrorResponse(header, data) =>
            context.log.error(s"ErrorResponse=>status=${header.status}")
            Behaviors.same
        })

    })
  }

  sealed trait Command

  final case object DiscoverSoundCam extends Command

  final case object SoundCamConnected extends Command

  final case object SoundCamConnectFailed extends Command

  // ConnectedCommand
  sealed trait ConnectedCmd extends Command
  final case class IdentificationResponse(protocol: Int, deviceId: Int, deviceMode: Int,
                                          deviceError: Int, serial: Int) extends ConnectedCmd

  final case class ErrorResponse(header: ResponseHeader, data: ByteString) extends ConnectedCmd

  final case object ConnectionClosed extends ConnectedCmd

  final case object ResetResponse extends ConnectedCmd

  final case object PrepareStateResponse extends ConnectedCmd

  final case object FinishStateResponse extends ConnectedCmd

  final case object StartProcedureResponse extends ConnectedCmd

  final case object StopProcedureResponse extends ConnectedCmd

  final case object WriteDataObjectResponse extends ConnectedCmd

  //final case class CommonStatus(deviceState:Int, subState:Int) extends Command
  //final case class Distance(value:Int) extends Command
  //final case class FrequencyRange(min:Int, max:Int) extends Command
  final case class CurrentDateTime(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int)
    extends ConnectedCmd

}
