package com.apaa


import akka.actor
import akka.actor.ActorRef
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{Behavior, PostStop}
import akka.util.ByteString
import com.apaa.SoundCamProtocol.ResponseHeader
import org.slf4j.LoggerFactory

import java.time.{Duration, Instant, LocalDateTime}
import scala.concurrent.duration.DurationInt

object SoundCamClient {
  val logger = LoggerFactory.getLogger(this.getClass)
  //#create-actors
  val system = actor.ActorSystem("ClassicSystem")

  def apply(): Behavior[Command] = unconnected(None)

  //#create-actors

  def unconnected(protocolOpt: Option[ActorRef]): Behavior[Command] = Behaviors.setup { context =>

    val protocol = protocolOpt.getOrElse({
      system.actorOf(SoundCamProtocol.props(context.self), "SoundCamProtocol")
    })

    val protocolParam = ProtocolParam(Protocol.tcp, Some("192.168.2.200"), None, None)
    system.actorOf(Duo.props(protocolParam), "duoActor")

    //val config = com.typesafe.config.ConfigFactory.defaultApplication()
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
        connected(protocol, context, DeviceState.Idle, DeviceSubState.Idle)

      case _ =>
        Behaviors.unhandled
    }.receiveSignal {
      case (context, PostStop) =>
        context.log.info("Client stopped.")
        system.terminate()
        Behaviors.same
    }
  }

  case object CommonStatusTimer

  def connected(protocol: ActorRef, context: ActorContext[Command],
                deviceState: Int, subState: Int): Behavior[Command] = {
    Behaviors.withTimers({
      timers =>
        // instead of FSM state timeout
        if (deviceState == DeviceState.Measuring && subState == DeviceSubState.Running) {
          timers.cancelAll()
        } else {
          if (!timers.isTimerActive(CommonStatusTimer)) {
            context.log.info("start timer...")
            timers.startTimerWithFixedDelay(CommonStatusTimer, ReadDataObject(Seq(DataObjectID.CommonStatus)), 9.seconds)
          }
        }

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

            case r@VideoData(timestamp, h, v, data) =>
              val begin = Instant.now()
              SoundCamInfoHandler.receive(r)
              val end = Instant.now()
              val duration = Duration.between(begin, end)
              logger.debug(s"video ${duration.getNano/1000000} ms")
              Behaviors.same

            case r@AcousticImage(timestamp, freqMin, freqMax, distance, data) =>
              SoundCamInfoHandler.receive(r)
              Behaviors.same

            case r@Spectrum(timestamp, delta, filted, freqMin, freqMax, globalSpectrum, localSpectrum) =>
              val begin = Instant.now()
              SoundCamInfoHandler.receive(r)
              val end = Instant.now()
              val duration = Duration.between(begin, end)
              logger.debug(s"spectrum ${duration.getNano/1000000} ms")
              Behaviors.same

            case r@(AudioData(_, _, _, _, _, _, _, _, _) | DataToSend(_, _, _, _, _, _, _, _, _, _)) =>
              //context.log.info(r.toString)
              Behaviors.same

            case dt: CurrentDateTime =>
              Behaviors.same
          }
        }

        def prepare() = {
          val now = LocalDateTime.now()
          val currentDateTime = CurrentDateTime(now.getYear, now.getMonthValue, now.getDayOfMonth,
            now.getHour, now.getMinute, now.getSecond, false)
          val dataObjects = Seq(
            Distance(350),
            FrequencyRange(5623, 40000),
            CameraResolution(640, 480),
            VideoFrameRate(30),
            AcousticFrameRate(30),
            currentDateTime
          )
          protocol ! SoundCamClient.WriteDataObject(dataObjects)
          protocol ! SoundCamClient.PrepareState(DeviceState.Measuring)
        }

        Behaviors.receiveMessage[Command]({
          // Connection
          case cmd: ConnectionCommand =>
            connectionCmdHandler(cmd)

          //Request
          case req@(IDRequest | Reset | PrepareState(_)
                    | StopProcedure | ReadDataObject(_)
                    | DiscoverSoundCam | FinishState |
                    WriteDataObject(_)) =>
            protocol ! req
            Behaviors.same

          case req@StartProcedure =>
            //timers.cancelAll()
            protocol ! req
            Behaviors.same

          // Response
          case resp: IdentificationResponse =>
            context.log.info(s"serial=${resp.serial}")
            prepare()
            Behaviors.same

          case StartProcedureResponse =>
            connected(protocol, context, DeviceState.Measuring, DeviceSubState.Running)

          case StopProcedureResponse =>
            connected(protocol, context, deviceState, DeviceSubState.Finished)

          case PrepareStateResponse =>
            protocol ! StartProcedure
            Behaviors.same

          // ignore resp
          case resp@(FinishStateResponse | ResetResponse |
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

  final case class CameraLight(on: Boolean) extends DataObject

  final case class LocalSoundTargetCoordinates(h: Short, v: Short) extends DataObject

  final case class DataToSend(acousticPicture: Boolean, videoPictures: Boolean, localSound: Boolean, micLevel: Boolean,
                              micRawData: Boolean, spectra: Boolean, singleMicData: Boolean, localSoundFiltered: Boolean,
                              levelFiltered: Boolean, rawDataFiltered: Boolean) extends DataObject

  final case class VideoData(timestamp: Long, h: Short, v: Short, data: ByteString) extends DataObject

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
