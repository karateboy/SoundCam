package com.apaa

import akka.util.ByteString
import com.apaa.OctaveBandTool.calculateAweight
import com.apaa.SoundCamClient.{AcousticImage, AudioData, Spectrum, VideoData}
import org.opencv.core.{Core, CvType, Mat, MatOfByte, Scalar, Size}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.slf4j.LoggerFactory
import scalafx.application.Platform
import scalafx.scene.control.Label
import scalafx.scene.image.{Image, ImageView}

import java.io.ByteArrayInputStream

object SoundCamInfoHandler {
  val logger = LoggerFactory.getLogger(this.getClass)
  var videoH = 640
  var videoV = 480
  var frameRate = 30

  case class Measurement(timestamp: Long,
                         video: Option[VideoData], acoustic: Option[AcousticImage],
                         spectrum: Option[Spectrum], audioData: Option[AudioData])
  //val capture = new VideoCapture()
  //capture.open(0)

  def receive(video: VideoData) = {
    for (sink <- videoSink) {
      val frame = new Mat(video.v, video.h, CvType.CV_8UC1)
      val bs = ByteString(video.data)
      frame.put(0, 0, bs.toArray)

      val buffer = new MatOfByte
      Imgcodecs.imencode(".png", frame, buffer);
      val img = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          //sink.setImage(img)
        }
      })
    }
  }

  def receive(ac: AcousticImage) = {

    for(sink <- videoSink) {
      val frame = new Mat(48, 64, CvType.CV_32FC1)
      frame.put(0, 0, ac.data)
      val resized = new Mat(videoV, videoH, CvType.CV_32FC1)

      Imgproc.resize(frame, resized, new Size(videoH, videoV))
      val ret = Core.minMaxLoc(resized)
      //logger.info(s"max=${ret.maxVal} min=${ret.minVal}")
      val threshold = new Mat()
      Imgproc.threshold(resized, threshold, ret.maxVal - 3.0, ret.maxVal, Imgproc.THRESH_TOZERO)
      val dest = new Mat()

      threshold.convertTo(dest, CvType.CV_8UC1)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      //Core.normalize(videoFrame,dest,ret.minVal,ret.maxVal,Core.NORM_MINMAX,CvType.CV_8SC3)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      val buffer = new MatOfByte
      Imgcodecs.imencode(".png", dest, buffer);
      val img = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          sink.setImage(img)
        }
      })
    }
  }

  def receive(spectrum: Spectrum) = {
    val globalFreqValues =
      for ((v, idx) <- spectrum.globalSpectrum.zipWithIndex) yield {
        FreqValue(spectrum.freqMin + spectrum.delta * idx, v)
      }

    val localFreqValues =
      for ((v, idx) <- spectrum.localSpectrum.zipWithIndex) yield {
        FreqValue(spectrum.freqMin + spectrum.delta * idx, v)
      }

    val globalDbA = calculateAweight(globalFreqValues)
    val localDbA = calculateAweight(localFreqValues)
    logger.info(s"local=${localDbA} global=${globalDbA}")
  }

  def receive(audioData: AudioData) = ???

  var videoSink: Option[ImageView] = None

  def setVideoSink(sink: ImageView) = {
    videoSink = Some(sink)
  }



}
