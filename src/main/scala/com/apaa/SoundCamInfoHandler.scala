package com.apaa

import akka.util.ByteString
import com.apaa.OctaveBandTool.sumAweight
import com.apaa.SoundCamClient.{AcousticImage, AudioData, Spectrum, VideoData}
import org.opencv.core.Core.MinMaxLocResult
import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.slf4j.{Logger, LoggerFactory}
import scalafx.application.Platform
import scalafx.scene.chart.{BarChart, LineChart, XYChart}
import scalafx.scene.control.TextField
import scalafx.scene.image.{Image, ImageView}

import java.io.ByteArrayInputStream
import java.time.Instant
import scala.collection.mutable.{ListBuffer, SortedMap}

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
  private var splChart:Option[LineChart[Number, Number]] = None

  def receive(video: VideoData): Unit = {
    val measurement: Measurement = getMeasurement(video.timestamp)
    measurement.videoOpt = Some(video)
    measureHandler(measurement)
    removeOldMeasurement()
  }

  private def videoHandler(video: VideoData, minMax: MinMaxLocResult): Unit = {
    for {
      sink <- videoSink
    } {
      val frame = new Mat(video.v, video.h, CvType.CV_8UC1)
      val bs = ByteString(video.data)
      frame.put(0, 0, bs.toArray)
      val colorFrame = new Mat()
      Imgproc.cvtColor(frame, colorFrame, Imgproc.COLOR_GRAY2RGBA, 3)
      Imgproc.drawMarker(colorFrame, minMax.maxLoc, new Scalar(0, 0, 255), 3)
      val buffer = new MatOfByte
      Imgcodecs.imencode(".png", colorFrame, buffer);
      val img = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          sink.setImage(img)
        }
      })
    }
  }

  def getMeasurement(timestamp: Long): Measurement = {
    val instant = timestampMap.getOrElseUpdate(timestamp, Instant.now())
    val measurement = dataMap.getOrElseUpdate(instant, Measurement(timestamp, None, None, None, None))
    measurement
  }

  def removeOldMeasurement(): Unit = {
    val preTrigger = Instant.now().minusSeconds(3)
    dataMap = dataMap.dropWhile(p => p._1.isBefore(preTrigger))
    timestampMap = timestampMap.dropWhile(p => p._2.isBefore(preTrigger))
    logger.debug(s"dataMap(${dataMap.size}) timestampMap(${timestampMap.size})")
  }

  def receive(ac: AcousticImage): Unit = {
    val measurement: Measurement = getMeasurement(ac.timestamp)
    measurement.acousticOpt = Some(ac)
    measureHandler(measurement)
    removeOldMeasurement()
  }

  private def measureHandler(me: Measurement): Unit = {
    for {video <- me.videoOpt
         ac <- me.acousticOpt
         } {
      val minMax = acousticHandler(ac)
      videoHandler(video, minMax)
    }
  }

  private def acousticHandler(ac: AcousticImage): MinMaxLocResult = {
    def showMinMax(frame: Mat, name: String): Core.MinMaxLocResult = {
      val ret = Core.minMaxLoc(frame)
      logger.info(s"$name max=${ret.maxVal} X=${ret.maxLoc.x} Y=${ret.maxLoc.y}")
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

      //val normalized = new Mat()
      //Core.normalize(threshold, normalized, 0, 255, Core.NORM_MINMAX)
      //showMinMax(normalized, "normalized")

      //val grayscaled = new Mat()
      // normalized.convertTo(grayscaled, CvType.CV_8UC1)

      // showMinMax(grayscaled, "grayscaled")

      /*
      val colored = new Mat()
      Imgproc.applyColorMap(grayscaled, colored, Imgproc.COLORMAP_HOT)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      //Core.normalize(videoFrame,dest,ret.minVal,ret.maxVal,Core.NORM_MINMAX,CvType.CV_8SC3)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)


      val buffer: MatOfByte = new MatOfByte
      Imgcodecs.imencode(".png", grayscaled, buffer);
      val img: Image = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          sink.setImage(img)
        }
      })

       */

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

  def setDuoDbA(duoDba: TextField): Unit = {
    duoDbaOpt = Some(duoDba)
  }

  def setSpectrumChart(chart: BarChart[String, Number]): Unit = {
    spectrumChart = Some(chart)
  }

  def setSplChart(chart:LineChart[Number, Number]) : Unit = {
    splChart = Some(chart)
  }

  val toLineChartData = (xy: (Int, Double)) => XYChart.Data[Number, Number](xy._1, xy._2)
  val toBarChartData = (xy: (String, Double)) => XYChart.Data[String, Number](xy._1, xy._2)

  val splList:ListBuffer[Double] = scala.collection.mutable.ListBuffer(0,0,0,0,0,0,0,0,0,0)
  // val timelabels = Seq("前8秒", "前7秒", "前6秒", "前5秒", "前4秒", "前3秒", "前2秒", "前1秒", "現在")
  val timelabels:Seq[Int] = Seq(-8, -7, -6, -5, -4, -3, -2, -1, 0)
  def receive(duoValues: DuoValues) = {
    Platform.runLater(new Runnable() {
      override def run(): Unit = {
        for (duoDba <- duoDbaOpt) {
          if (duoValues.instantValues.nonEmpty) {
            duoDba.setText(s"${duoValues.instantValues(3)}")
            splList.remove(0)
            splList.append(duoValues.instantValues(3))
            for(splChart <-splChart){
              val spectrum: Seq[(Int, Double)] = timelabels.zip(splList)
              val series1 = new XYChart.Series[Number, Number] {
                name = "LAF"
                data = spectrum.map(toLineChartData)
              }
              splChart.data = series1
            }
          }
        }

        for (spectrumChart <- spectrumChart) {
          val spectrum: Seq[(String, Double)] = Duo.ONE_THIRD_OCTAVE_BANDS_CENTER_FREQ.zip(duoValues.spectrum)
          val series1 = new XYChart.Series[String, Number] {
            name = "1/3 頻譜"
            data = spectrum.map(toBarChartData)
          }
          spectrumChart.data = series1
        }
      }
    })
  }

  case class Measurement(timestamp: Long,
                         var videoOpt: Option[VideoData], var acousticOpt: Option[AcousticImage],
                         var spectrum: Option[Spectrum], var audioData: Option[AudioData])

}
