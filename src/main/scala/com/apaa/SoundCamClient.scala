package com.apaa


import akka.actor
import akka.actor.ActorRef
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{Behavior, PostStop}
import akka.util.ByteString
import com.apaa.SoundCamProtocol.ResponseHeader

import java.nio.ByteBuffer
import scala.concurrent.duration.DurationInt

object SoundCamClient {

  //#create-actors
  val system = actor.ActorSystem("ClassicSystem")

  def apply(): Behavior[Command] = unconnected(None)

  //#create-actors

  def unconnected(protocolOpt: Option[ActorRef]): Behavior[Command] = Behaviors.setup { context =>

    val protocol = protocolOpt.getOrElse({
      system.actorOf(SoundCamProtocol.props(context.self), "SoundCamProtocol")
    })

    val config = com.typesafe.config.ConfigFactory.defaultApplication()
    //config.get
    protocol ! DiscoverSoundCam

    Behaviors.receiveMessage[Command] {
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
    }.receiveSignal {
      case (context, PostStop) =>
        context.log.info("Client stopped.")
        system.terminate()
        Behaviors.same
    }
  }

  def connected(protocol: ActorRef, context: ActorContext[Command], deviceState: Int, subState: Int): Behavior[Command] = {
    Behaviors.withTimers({
      timers =>
        // instead of FSM state timeout
        if (deviceState != DeviceState.Measuring)
          timers.startTimerWithFixedDelay(context.self, ReadDataObject(Seq(DataObjectID.CommonStatus)), 9.seconds)

        def connectionCmdHandler(cmd: ConnectionCommand): Behavior[Command] = {
          cmd match {
            case cmd@SoundCamConnectFailed =>
              context.log.debug(cmd.toString)
              timers.cancelAll()
              unconnected(Some(protocol))
            case _ =>
              Behaviors.same
          }
        }

        def dataObjectHandler(dataObject: DataObject): Behavior[Command] = {
          dataObject match {
            case r@CommonStatus(errorCode, newDeviceState, newSubState) =>
              context.log.debug(r.toString)
              if (errorCode == 0 && (deviceState != newDeviceState || subState != newSubState)) {
                connected(protocol, context, newDeviceState, newSubState)
              } else
                Behaviors.same

            case r@CameraStatus(dateTime, temperature) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@Distance(value) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@FrequencyRange(min, max) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@CameraResolution(v, h) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@CameraBrightness(b) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@AcousticFrameRate(rate) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@VideoFrameRate(rate) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@LocalSoundTargetCoordinates(h, v) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@CameraLight(on) =>
              context.log.debug(r.toString)
              Behaviors.same

            case r@VideoData(timestamp, h, v, data)=>
              context.log.debug(r.toString)
              Behaviors.same

            case r@(AcousticImage(_, _, _, _, _) | AudioData(_, _, _, _, _, _, _, _, _) | DataToSend(_, _, _, _, _, _, _, _, _, _)
                 | Spectrum(_, _, _, _, _, _, _)) =>
              context.log.debug(r.toString)
              Behaviors.same

            case dt: CurrentDateTime =>
              Behaviors.same
          }
        }

        Behaviors.receiveMessage[Command]({
          // Connection
          case cmd: ConnectionCommand =>
            connectionCmdHandler(cmd)
          //Request
          case req@(IDRequest | Reset | PrepareState(_)
                    | StartProcedure | StopProcedure | ReadDataObject(_)
                    | DiscoverSoundCam | FinishState |
                    WriteDataObject(_)) =>
            protocol ! req
            Behaviors.same

          // Response
          case resp: IdentificationResponse =>
            context.log.info(s"serial=${resp.serial}")
            //protocol ! ReadDataObject(Seq(DataObjectID.DataToSend))
            Behaviors.same

          // ignore resp
          case resp@(PrepareStateResponse | StartProcedureResponse
                     | StopProcedureResponse | FinishStateResponse | ResetResponse |
                     WriteDataObjectResponse) =>
            context.log.debug(resp.toString)
            Behaviors.same

          // Data Object
          case dataObject: DataObject =>
            dataObjectHandler(dataObject)

          case ErrorResponse(header, data) =>
            context.log.error(s"ErrorResponse=>status=${header.status}")
            Behaviors.same
        }).receiveSignal {
          case (context, PostStop) =>
            context.log.info("Client stopped.")
            system.terminate()
            Behaviors.same
        }
    })
  }

  sealed trait Command

  sealed trait ConnectionCommand extends Command

  sealed trait DataObject extends Command

  final case class PrepareState(state: Int) extends Command

  final case class IdentificationResponse(protocol: Int, deviceId: Int, deviceMode: Int,
                                          deviceError: Int, serial: Int) extends Command

  final case class ErrorResponse(header: ResponseHeader, data: ByteString) extends Command

  final case class ReadDataObject(dataObjectID: Seq[Int]) extends Command

  final case class WriteDataObject(dataObjects: Seq[DataObject]) extends Command

  // Data Object
  final case class CommonStatus(errorCode: Int, deviceState: Int, subState: Int) extends DataObject

  final case class CameraStatus(dateTime: CurrentDateTime, temperature: Float) extends DataObject

  final case class Distance(value: Int) extends DataObject

  final case class FrequencyRange(min: Int, max: Int) extends DataObject

  final case class CameraResolution(h: Short, v: Short) extends DataObject

  final case class CameraBrightness(b: Int) extends DataObject

  final case class AcousticFrameRate(rate: Int) extends DataObject

  final case class VideoFrameRate(rate: Int) extends DataObject

  final case class CameraLight(on:Boolean) extends  DataObject

  final case class LocalSoundTargetCoordinates(h: Short, v: Short) extends DataObject

  final case class DataToSend(acousticPicture: Boolean, videoPictures: Boolean, localSound: Boolean, micLevel: Boolean,
                              micRawData: Boolean, spectra: Boolean, singleMicData: Boolean, localSoundFiltered: Boolean,
                              levelFiltered: Boolean, rawDataFiltered: Boolean) extends DataObject

  final case class VideoData(timestamp: Long, h: Short, v: Short, data: ByteBuffer) extends DataObject

  final case class AcousticImage(timestamp: Long, freqMin: Short, freqMax: Short,
                                 distance: Int, data: Array[Float]) extends DataObject

  final case class Spectrum(timestamp: Long, delta: Float, filted: Boolean, freqMin: Int, freqMax: Int,
                            globalSpectrum: Array[Float], localSpectrum: Array[Float]) extends DataObject

  final case class AudioData(timestamp: Long, h: Short, v: Short,
                             dataPlus: Array[Int], dataMinus: Array[Int], dt: Double, filtered: Boolean,
                             freqMin: Int, freqMax: Int) extends DataObject

  final case class CurrentDateTime(year: Int, month: Int, day: Int, hour: Int, min: Int, sec: Int, utc: Boolean)
    extends DataObject

  // Connection Command
  final case object DiscoverSoundCam extends ConnectionCommand

  final case object SoundCamConnected extends ConnectionCommand

  final case object SoundCamConnectFailed extends ConnectionCommand

  // SoundCam protocol request/response
  final case object IDRequest extends Command

  final case object Reset extends Command

  final case object FinishState extends Command

  final case object StartProcedure extends Command

  final case object StopProcedure extends Command

  final case object ResetResponse extends Command

  final case object PrepareStateResponse extends Command

  final case object FinishStateResponse extends Command

  final case object StartProcedureResponse extends Command

  final case object StopProcedureResponse extends Command

  final case object WriteDataObjectResponse extends Command

}
