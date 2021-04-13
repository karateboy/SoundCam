package com.apaa

import akka.util.ByteString
import com.apaa.OctaveBandTool.sumAweight
import com.apaa.SoundCamClient.{AcousticImage, AudioData, Spectrum, VideoData}
import org.opencv.core._
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.imgproc.Imgproc
import org.slf4j.LoggerFactory
import scalafx.application.Platform
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
      def showMinMax(frame:Mat,name:String)={
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
      Imgproc.threshold(resized, threshold, resizedMinMax.maxVal-1, resizedMinMax.maxVal, Imgproc.THRESH_TOZERO)
      showMinMax(threshold, "threshold")

      val normalized = new Mat()
      Core.normalize(threshold, normalized, 0, 255, Core.NORM_MINMAX)
      showMinMax(normalized, "normalized")

      val grayscaled = new Mat()
      normalized.convertTo(grayscaled, CvType.CV_8UC1)

      val ret=showMinMax(grayscaled, "grayscaled")

      val colored = new Mat()
      Imgproc.applyColorMap(grayscaled, colored, Imgproc.COLORMAP_JET)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      //Core.normalize(videoFrame,dest,ret.minVal,ret.maxVal,Core.NORM_MINMAX,CvType.CV_8SC3)
      //Imgproc.drawMarker(dest, ret.maxLoc, new Scalar(255, 0, 0), 3)

      val buffer = new MatOfByte
      Imgcodecs.imencode(".png", colored, buffer);
      val img = new Image(new ByteArrayInputStream(buffer.toArray()))
      Platform.runLater(new Runnable() {
        override def run(): Unit = {
          sink.setImage(img)
        }
      })
    }
  }

  def receive(spectrum: Spectrum) = {
    val globalDbA = sumAweight(spectrum.globalSpectrum)
    val localDbA = sumAweight(spectrum.localSpectrum)
    logger.info(s"local=${localDbA} global=${globalDbA}")
  }

  def receive(audioData: AudioData) = ???

  var videoSink: Option[ImageView] = None

  def setVideoSink(sink: ImageView) = {
    videoSink = Some(sink)
  }



}
