package com.apaa

import com.apaa.pages.{ConfigurePage, OperationPage}
import scalafx.ensemble.commons.DisplayablePage
import scalafx.ensemble.stage.DashboardPage
import scalafx.scene.control._


case class ThumbNail(button: Button)

case class PageItem(label: String, page: Option[DisplayablePage]) {
  override def toString: String = label
}


object SortUtils {

  def treeItemSort: (TreeItem[String], TreeItem[String]) => Boolean = (ti: TreeItem[String], t2: TreeItem[String]) =>
    compare(ti.value(), t2.value())

  def thumbNailsSort: (ThumbNail, ThumbNail) => Boolean = (t1: ThumbNail, t2: ThumbNail) =>
    compare(t1.button.text(), t2.button.text())

  def sortKeys: (String, String) => Boolean = (x: String, y: String) => compare(x, y)

  private def compare = (x: String, y: String) =>
    x.compareToIgnoreCase(y) < 0
}

object SoundCamTree {
  val dashboardPage = new DashboardPage()
  val rootLabel = "聲音相機"
  val configurePage = new ConfigurePage()
  val operationPage = new OperationPage()
  val tree: List[TreeItem[PageItem]] = List(
    new TreeItem[PageItem](PageItem(configurePage.getTitle, Some(configurePage.getDisplayablePage))) {
      children = List.empty[TreeItem[PageItem]]
    },
    new TreeItem[PageItem](PageItem(operationPage.getTitle, Some(operationPage.getDisplayablePage))) {
      children = List.empty[TreeItem[PageItem]]
    },
    new TreeItem[PageItem](PageItem("報表", None)) {
      children = List.empty[TreeItem[PageItem]]
    },
  )

  val root: TreeItem[PageItem] = new TreeItem[PageItem](PageItem(rootLabel, Some(dashboardPage))) {
    expanded = true
    children = tree
  }

}
