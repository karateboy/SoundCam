//#full-example
package com.apaa

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorSystem, Behavior}
import org.scalafx.extras.{onFXAndWait, showException}
import scalafx.Includes._
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.ensemble.commons.DisplayablePage
import scalafx.ensemble.stage.DashboardPage
import scalafx.geometry.Insets
import scalafx.scene.control._
import scalafx.scene.image.{Image, ImageView}
import scalafx.scene.layout._
import scalafx.scene.{Node, Scene}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

//#main-class
object SoundCamApp extends JFXApp3 {

  sealed trait Command

  case object Stop extends Command

  def apply(): Behavior[Command] =
    Behaviors.setup {
      context =>
        context.spawn(SoundCamClient(), "soundCamClient")
        Behaviors.receiveMessage({
          case Stop =>
            Behaviors.stopped
        })
    }

  val soundCamApp: ActorSystem[SoundCamApp.Command] = ActorSystem(SoundCamApp(), "SoundCamApp")

  private val Title = "CAE聲音相機"

  private var _centerPane: Node = _

  def centerPane: Node = _centerPane

  def centerPane_=(newValue: Node): Unit = _centerPane = newValue

  private[apaa] lazy val splitPane: SplitPane = new SplitPane {
    dividerPositions = 0
    id = "page-splitpane"
    items.addAll(scrollPane, centerPane)
  }

  private lazy val scrollPane: ScrollPane = new ScrollPane {
    minWidth = 200
    maxWidth = 200
    fitToWidth = true
    fitToHeight = true
    id = "page-tree"
    content = controlsView
  }

  private lazy val controlsView: TreeView[PageItem] = new TreeView[PageItem]() {
    minWidth = 200
    maxWidth = 200
    editable = true
    root = rootTreeItem
    id = "page-tree"
  }

  private[apaa] lazy val rootTreeItem: TreeItem[PageItem] = SoundCamTree.root

  override def start(): Unit = {

    setupUncaughtExceptionHandling()

    //
    // Example selection tree
    //
    val dashboardPage = new DashboardPage()
    centerPane = PageDisplayer.showPage(dashboardPage)


    controlsView.selectionModel().setSelectionMode(SelectionMode.Single)
    controlsView.selectionModel().selectedItemProperty.onChange {
      (_, _, newItem) => {

        val pageOpt: Option[DisplayablePage] = (newItem.isLeaf, Option(newItem.getParent)) match {
          case (true, Some(_)) => newItem.getValue.page
          case (false, Some(_)) => Some(dashboardPage)
          case (_, _) => Some(dashboardPage)
        }
        for(page <-pageOpt){
          centerPane = PageDisplayer.showPage(page)
          splitPane.items.remove(1)
          splitPane.items.add(1, centerPane)
          // Update layout after updating content
          splitPane.autosize()
        }
      }
    }

    //
    // Layout the main stage
    //
    stage = new PrimaryStage {
      title = Title
      icons += new Image("/scalafx/ensemble/images/ScalaFX-icon-64x64.png")
      scene = new Scene(1020, 700) {
        stylesheets += this.getClass.getResource("/scalafx/ensemble/css/ensemble.css").toExternalForm
        root = new BorderPane {
          top = new VBox {
            vgrow = Priority.Always
            hgrow = Priority.Always
            children = new ToolBar {
              prefHeight = 76
              maxHeight = 76
              id = "mainToolBar"
              content = List(
                new ImageView {
                  image = new Image(
                    this.getClass.getResourceAsStream("/scalafx/ensemble/images/logo.png"))
                  margin = Insets(0, 0, 0, 10)
                },
                new Region {
                  minWidth = 20
                },
                new Button {
                  minWidth = 120
                  minHeight = 66
                  id = "newButton"
                })
            }
          }
          center = new BorderPane {
            center = splitPane
          }
          styleClass += "application"
        }
      }
    }
  }

  def setupUncaughtExceptionHandling(): Unit = {
    Thread.setDefaultUncaughtExceptionHandler(
      (t: Thread, e: Throwable) => {
        showException(Title, s"Unhandled exception on thread ${t.getName}.", e, stage)
      }
    )


    onFXAndWait {
      Thread.currentThread().setUncaughtExceptionHandler(
        (t: Thread, e: Throwable) => {
          showException(Title, s"Unhandled exception on thread ${t.getName}.", e, stage)
        }
      )
    }
  }

  override def stopApp(): Unit = {
    println("SoundCam stop!")
    soundCamApp.terminate()
    Await.ready(soundCamApp.whenTerminated, Duration.Inf)
    println("actorSystem shutdown!")
  }
}
