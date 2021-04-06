package com.apaa

import com.apaa.SoundCamClient.{AcousticImage, AudioData, Spectrum, VideoData}
import org.opencv.core.{CvType, Mat}
import org.opencv.imgcodecs.Imgcodecs
import org.opencv.videoio.VideoCapture
import scalafx.application.Platform
import scalafx.scene.image.{Image, ImageView}

import java.io.ByteArrayInputStream

object SoundCamInfoHandler {
  //val capture = new VideoCapture()
  //capture.open(0)

  def receive(video:VideoData) = {
    for(sink <- videoSink){
      val frame = new Mat(video.v, video.h, CvType.CV_8UC1)
      frame.put(0,0, video.data.array())
      //val frame = new Mat
      //capture.read(frame)
      import org.opencv.core.MatOfByte
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
  def receive(acousticImage: AcousticImage) = ???
  def receive(spectrum: Spectrum) = ???
  def receive(audioData: AudioData) = ???

  var videoSink: Option[ImageView] = None
  def setVideoSink(sink:ImageView) = {
    videoSink = Some(sink)
  }



}
