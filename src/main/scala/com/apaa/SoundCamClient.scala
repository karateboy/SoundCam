package com.apaa


import akka.actor
import akka.actor.ActorRef
import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.util.ByteString
import com.apaa.SoundCamProtocol.{FinishState, IDRequest, PrepareState, ReadDataObject, ResponseHeader, StartProcedure, StopProcedure}

import java.nio.{ByteBuffer, ByteOrder}
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
      case r@DiscoverSoundCam =>
        context.log.debug(r.toString)
        protocol ! DiscoverSoundCam
        Behaviors.same

      case SoundCamConnectFailed =>
        context.log.error("SoundCamConnectFailed")
        Behaviors.same

      case r@SoundCamConnected =>
        context.log.debug(r.toString)
        protocol ! IDRequest
        connected(protocol, context, 0, 0)

      case _ =>
        Behaviors.unhandled
    }
  }

  def connected(protocol: ActorRef, context: ActorContext[Command], deviceState:Int, subState:Int): Behavior[Command] = {
    Behaviors.withTimers({
      timers =>
        // instead of FSM state timeout
        timers.startTimerWithFixedDelay(context.self, ReadDataObjectRequest(Seq(DataObjectID.CommonStatus)), 9.seconds)

        Behaviors.receiveMessagePartial({

          case r@ConnectionClosed =>
            context.log.debug(r.toString)
            timers.cancelAll()
            unconnected(Some(protocol))

          case response: IdentificationResponse =>
            context.log.info(s"serial=${response.serial}")
            protocol ! ReadDataObject(Seq(DataObjectID.Distance))
            Behaviors.same

          case ReadDataObjectRequest(dataObjectIDs) =>
            protocol ! ReadDataObject(dataObjectIDs)
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

          case r@CommonStatus(errorCode, deviceState, subState)=>
            context.log.debug(r.toString)
            Behaviors.same

          case r@CameraStatus(dateTime, temperature) =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@Distance(value) =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@FrequencyRange(min, max)=>
            context.log.debug(r.toString)
            Behaviors.same

          case r@CameraResolution(v,h)=>
            context.log.debug(r.toString)
            Behaviors.same

          case r@CameraBrightness(b)=>
            context.log.debug(r.toString)
            Behaviors.same

          case r@AcousticFrameRate(rate) =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@VideoFrameRate(rate) =>
            context.log.debug(r.toString)
            Behaviors.same

          case r@LocalSoundTargetCoordinates(h,v)=>
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

  final case class ReadDataObjectRequest(dataObjectID: Seq[Int]) extends ConnectedCmd

  final case class CommonStatus(errorCode:Int, deviceState:Int, subState:Int) extends Command
  final case class CameraStatus(dateTime:CurrentDateTime, temperature:Float) extends Command

  final case class Distance(value:Int) extends Command
  final case class FrequencyRange(min:Int, max:Int) extends Command
  final case class CameraResolution(h:Short, v:Short) extends Command
  final case class CameraBrightness(b:Int) extends Command
  final case class AcousticFrameRate(rate:Int) extends Command
  final case class VideoFrameRate(rate:Int) extends Command
  final case class LocalSoundTargetCoordinates(h:Short, v:Short) extends Command
  final case class DataToSend(acousticPicture:Boolean, videoPictures:Boolean, localSound:Boolean, micLevel:Boolean,
                              micRawData:Boolean, spectra:Boolean, singleMicData:Boolean, localSoundFiltered:Boolean,
                              levelFiltered:Boolean, rawDataFiltered:Boolean) extends Command
  final case class VideoData(timestamp:Long, h:Short, v:Short, data:ByteBuffer) extends Command
  final case class AcousticImage(timestamp:Long, freqMin:Short, freqMax:Short,
                                 distance:Int, data:Seq[Float]) extends Command
  final case class Spectrum(timestamp:Long, delta:Float, filted: Boolean, freqMin:Int, freqMax:Int,
                            globalSpectrum: Seq[Float], localSpectrum:Seq[Float]) extends Command
  final case class AudioData(timestamp:Long, h:Short, v:Short,
                             dataPlus:Seq[Int], dataMinus:Seq[Int], dt:Double, filtered:Boolean,
                             freqMin:Int, freqMax:Int) extends Command

  final case class CurrentDateTime(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int, utc:Boolean)
    extends ConnectedCmd

}
