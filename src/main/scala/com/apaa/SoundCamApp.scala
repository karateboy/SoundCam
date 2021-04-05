//#full-example
package com.apaa

import akka.actor.typed.scaladsl.AskPattern.{Askable, schedulerFromActorSystem}
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import com.apaa.SoundCamApp.soundCamApp.log
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

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

//#main-class
object SoundCamApp extends JFXApp3 {
  // #ScalaFX UI
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
  implicit val soundCamApp: ActorSystem[SoundCamApp.Command] = ActorSystem(SoundCamApp(), "SoundCamApp")
  implicit val timeout: Timeout = 3.seconds
  implicit val ec = soundCamApp.executionContext
  private lazy val controlsView: TreeView[PageItem] = new TreeView[PageItem]() {
    minWidth = 200
    maxWidth = 200
    editable = true
    root = rootTreeItem
    id = "page-tree"
  }
  private[apaa] lazy val rootTreeItem: TreeItem[PageItem] = SoundCamTree.root
  private val Title = "CAE聲音相機"
  private var _soundCamClient: ActorRef[SoundCamClient.Command] = _
  private var _centerPane: Node = _

  // #actor
  def apply(): Behavior[Command] =
    Behaviors.setup {
      context =>
        val client = context.spawn(SoundCamClient(), "soundCamClient")
        Behaviors.receiveMessage({
          case GetClient(replyTo) =>
            replyTo ! Client(client)
            Behaviors.same
          case Stop =>
            Behaviors.stopped
        })
    }

  def soundCamClient = _soundCamClient

  override def start(): Unit = {
    import org.opencv.core.Core
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
    readConfig()

    initActorRef()

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
        for (page <- pageOpt) {
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

  def initActorRef() = {
    val result: Future[SoundCamApp.Reply] = soundCamApp.ask(ref => SoundCamApp.GetClient(ref))
    result.onComplete({
      case Success(SoundCamApp.Client(client)) => _soundCamClient = client
      case Failure(exception) => log.error("fail to get client ref", exception)
    })
  }

  def readConfig () = {
    val config = com.typesafe.config.ConfigFactory.load()

  }
  def centerPane: Node = _centerPane

  def centerPane_=(newValue: Node): Unit = _centerPane = newValue

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
    soundCamApp.terminate()
  }

  sealed trait Command

  sealed trait Reply

  case class GetClient(replyTo: ActorRef[Reply]) extends Command

  final case class Client(client: ActorRef[SoundCamClient.Command]) extends Reply

  case object Stop extends Command

}
