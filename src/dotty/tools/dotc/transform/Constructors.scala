package dotty.tools.dotc
package transform

import core._
import Phases._
import Contexts._
import Symbols._
import parsing.Parsers.Parser
import config.Printers._
import Denotations._

class Constructors extends Phase {

   def name = "constructors"

   override def description =
    """Creates the primary constructor from the class definition"""

   override def run(implicit ctx: Context): Unit = {

   }

}