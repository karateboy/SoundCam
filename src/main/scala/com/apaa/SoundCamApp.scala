//#full-example
package com.apaa

import akka.actor.typed.{ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

//#main-class
object SoundCamApp extends App {
  sealed trait Command
  def apply(): Behavior[Command] =
    Behaviors.setup {
      context =>
        val client = context.spawn(SoundCamClient(), "soundCamClient")
        Behaviors.ignore
    }

  val soundCamApp: ActorSystem[SoundCamApp.Command] = ActorSystem(SoundCamApp(), "SoundCamApp")

}
