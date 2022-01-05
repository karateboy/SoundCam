package com.apaa

import akka.util.ByteString
import com.apaa.OctaveBandTool.sumAweight
import com.apaa.SoundCamClient.{AcousticImage, AudioData, Spectrum, VideoData}
import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.slf4j.{Logger, LoggerFactory}
import scalafx.application.Platform
import scalafx.scene.chart.{BarChart, XYChart}
import scalafx.scene.control.TextField
import scalafx.scene.image.{Image, ImageView}

import java.io.ByteArrayInputStream
import java.time.Instant
import scala.collection.SortedMap

object SoundCamInfoHandler {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)
  var videoH = 640
  var videoV = 480
  var frameRate = 30
  var timestampMap: SortedMap[Long, Instant] = SortedMap.empty[Long, Instant]

  var dataMap: SortedMap[Instant, Measurement] = SortedMap.empty[Instant, Measurement]
  private var videoSink: Option[ImageView] = None
  private var acousticSink: Option[ImageView] = None
  private var globalDba: Option[TextField] = None
  private var localDba: Option[TextField] = None
  private var duoDbaOpt: Option[TextField] = None
  private var spectrumChart: Option[BarChart[String, Number]] = None

  def receive(video: VideoData): Unit = {
    val measurement: Measurement = getMeasurement(video.timestamp)
    measurement.video = Some(video)
    videoHandler(video)
    removeOldMeasurement()
  }

  private def videoHandler(video: VideoData): Unit = {
    for {
      sink <- videoSink
    } {
      val frame = new Mat(video.v, video.h, CvType.CV_8UC1)
      val bs = ByteString(video.data)
      frame.put(0, 0, bs.toArray)

      val buffer = new MatOfByte
      Imgcodecs.imencode(".png", frame, buffer);
      val img = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          sink.setImage(img)
        }
      })
    }
  }

  def getMeasurement(timestamp: Long): Measurement = {
    val instant = timestampMap.getOrElse(timestamp, Instant.now())
    timestampMap = timestampMap ++ Map(timestamp -> instant)
    val measurement = dataMap.getOrElse(instant, Measurement(timestamp, None, None, None, None))
    dataMap = dataMap ++ Map(instant -> measurement)
    measurement
  }

  def removeOldMeasurement(): Unit = {
    val preTrigger = Instant.now().minusSeconds(3)
    dataMap = dataMap.dropWhile(p => p._1.isBefore(preTrigger))
    timestampMap = timestampMap.dropWhile(p => p._2.isBefore(preTrigger))
    logger.debug(s"dataMap(${dataMap.size}) timestampMap(${timestampMap.size})")
  }

  def receive(ac: AcousticImage): Unit = {
    val measurement = getMeasurement(ac.timestamp)
    measurement.acoustic = Some(ac)
    acousticHandler(ac)
    removeOldMeasurement()
  }

  private def acousticHandler(ac: AcousticImage): Unit = {
    for (sink <- acousticSink) {
      def showMinMax(frame: Mat, name: String) = {
        val ret = Core.minMaxLoc(frame)
        logger.info(s"$name max=${ret.maxVal} min=${ret.minVal}")
        ret
      }

      val original = new Mat(48, 64, CvType.CV_32FC1)
      original.put(0, 0, ac.data)
      showMinMax(original, "frame")

      val resized = new Mat(videoV, videoH, CvType.CV_32FC1)
      Imgproc.resize(original, resized, new Size(videoH, videoV))
      val resizedMinMax = showMinMax(resized, "resized")

      val threshold = new Mat()
      Imgproc.threshold(resized, threshold, resizedMinMax.maxVal - 1, resizedMinMax.maxVal, Imgproc.THRESH_TOZERO)
      showMinMax(threshold, "threshold")

      val normalized = new Mat()
      Core.normalize(threshold, normalized, 0, 255, Core.NORM_MINMAX)
      showMinMax(normalized, "normalized")

      val grayscaled = new Mat()
      normalized.convertTo(grayscaled, CvType.CV_8UC1)

      //val ret = showMinMax(grayscaled, "grayscaled")

      val colored = new Mat()
      Imgproc.applyColorMap(grayscaled, colored, Imgproc.COLORMAP_JET)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      //Core.normalize(videoFrame,dest,ret.minVal,ret.maxVal,Core.NORM_MINMAX,CvType.CV_8SC3)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      val buffer: MatOfByte = new MatOfByte
      Imgcodecs.imencode(".png", colored, buffer);
      val img: Image = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          sink.setImage(img)
        }
      })
    }
  }

  def receive(spectrum: Spectrum): Unit = {
    val measurement = getMeasurement(spectrum.timestamp)
    measurement.spectrum = Some(spectrum)
    spectrumHandler(spectrum)
    removeOldMeasurement()
  }

  private def spectrumHandler(spectrum: Spectrum): Unit = {
    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        for {
          global <- globalDba
          local <- localDba
        } {
          global.setText(String.format("%.2f", sumAweight(spectrum.globalSpectrum)))
          local.setText(String.format("%.2f", sumAweight(spectrum.localSpectrum)))
        }
      }
    })
  }

  def receive(audioData: AudioData): Unit = {
    val measurement = getMeasurement(audioData.timestamp)
    measurement.audioData = Some(audioData)
    removeOldMeasurement()
  }

  def setVideoSink(sink: ImageView): Unit = {
    videoSink = Some(sink)
  }

  def setAcousticSink(sink: ImageView): Unit = {
    acousticSink = Some(sink)
  }

  def setDbaTextField(global: TextField, local: TextField): Unit = {
    globalDba = Some(global)
    localDba = Some(local)
  }

  def setDuoDbA(duoDba: TextField)={
    duoDbaOpt = Some(duoDba)
  }

  def setSpectrumChart(chart:BarChart[String, Number]) = {
    spectrumChart = Some(chart)
  }

  val toChartData = (xy: (String, Double)) => XYChart.Data[String, Number](xy._1, xy._2)

  def receive(duoValues: DuoValues) = {
    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        for(duoDba <- duoDbaOpt){
          if(duoValues.instantValues.nonEmpty){
            duoDba.setText(s"${duoValues.instantValues(0)}")
          }
        }

        for(spectrumChart <- spectrumChart){
          val spectrum: Seq[(String, Double)] = Duo.ONE_THIRD_OCTAVE_BANDS_CENTER_FREQ.zip(duoValues.spectrum)
          val series1 = new XYChart.Series[String, Number] {
            name = "1/3 頻譜"
            data = spectrum.map(toChartData)
          }
          spectrumChart.data = series1
        }
      }
    })
  }

  case class Measurement(timestamp: Long,
                         var video: Option[VideoData], var acoustic: Option[AcousticImage],
                         var spectrum: Option[Spectrum], var audioData: Option[AudioData])

}
