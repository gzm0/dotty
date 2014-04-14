package test.transform

import org.junit.Test
import test.DottyTest
import org.junit.Assert

class ConstructorsTest extends DottyTest {

  @Test
  def printTest = checkCompile("constructors",
      """
      class A(x: Int) {

        def this() = this(4)

      }
      """
  ) { (tree, context) =>

    implicit val ctx = context

    println(tree.show)

  }

}
