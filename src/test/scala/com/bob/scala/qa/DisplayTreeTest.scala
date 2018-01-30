package com.bob.scala.qa

import org.scalatest.FunSuite

class DisplayTreeTest extends FunSuite {

  test("test-one") {

    val l = DisplayTree.asciiDisplay(TreeNode("Root",
      children = List(TreeNode("level1-1"),
        TreeNode("level1-2"),
        TreeNode("level1-3"))))

    assert(List("+-Root", "  +-level1-1", "  +-level1-2", "  +-level1-3")
      == l)
  }
}