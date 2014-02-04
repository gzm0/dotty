package dotc

import org.junit.Test
import test._

class flattenTests extends CompilerTest {

  val posDir = "tests/pos/"
  val negDir = "tests/neg/"
  val dotcDir = "src/dotty/"

  @Test def pos_nestedclass() = compileFile(posDir, "nestedclass")


}