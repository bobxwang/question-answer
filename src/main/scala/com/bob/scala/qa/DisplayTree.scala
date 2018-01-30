package com.bob.scala.qa

case class TreeNode[T](data: T, children: Seq[TreeNode[T]] = Nil)

object DisplayTree extends App {

  def asciiDisplay(root: TreeNode[String]): Seq[String] = {

    def doDisplay(node: TreeNode[String],
                  level: Int): Seq[String] = {
      val s = level match {
        case l if (l >= 2) => Seq(s"  | ${" " * 2 * (l - 2)}+-${node.data}")
        case _ => Seq(s"${" " * 2 * level}+-${node.data}")
      }

      node.children match {
        case Nil => s
        case _ =>
          node.children.foldLeft(s) { (acc, c) =>
            if (c.children != Nil && level == 0) {
              acc ++ doDisplay(c, level + 1) ++ Seq("  |")
            } else {
              acc ++ doDisplay(c, level + 1)
            }
          }
      }
    }

    val result = doDisplay(root, 0)
    result.last match {
      case "  |" =>
        result.dropRight(1)
      case _ =>
        result
    }
  }

  asciiDisplay(TreeNode("Root",
    children = List(
      TreeNode("level1-1", children = TreeNode("level2-1", children = TreeNode("level3-1") :: Nil) :: Nil),
      TreeNode("level1-2"),
      TreeNode("level1-3"))))
    .foreach(println)
}