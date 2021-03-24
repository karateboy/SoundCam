//#full-example
package com.apaa

import akka.actor.typed.ActorSystem

//#main-class
object SoundCamApp extends App {
  val soundCamClient: ActorSystem[SoundCamClient.Command] = ActorSystem(SoundCamClient(), "SoundCamClient")

  soundCamClient ! SoundCamClient.ConnectSoundCam("", 1234)
}
