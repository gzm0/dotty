package dotty.tools.dotc
package transform

import scala.collection.mutable

import core._
import Phases._
import Contexts._
import Symbols._
import parsing.Parsers.Parser
import config.Printers._

class Flatten extends Phase {

  import ast.tpd
  import ast.Trees._

  // todo: core.Phases.PhasesBase also defines "flatten"
  def name = "flatten"

  override def description =
    """The flatten phase lifts nested classes to the package level"""

  override def run(implicit ctx: Context): Unit = {
    val flatter = new TreeFlattener
    val oldTree = ctx.compilationUnit.tpdTree
    val newTree = flatter.transform(oldTree)
    println("Tree after flatten")
    println(newTree.show)
    ctx.compilationUnit.tpdTree = newTree
  }

  /** TreeFlattener moves nested class definitions to the package level */
  class TreeFlattener extends tpd.TreeTransformer {

    /** Intermediate store for class definition that are about to be lifted
     *
     *  todo: this map is copied from Scalac 2.x flatten. Why do we need this
     *  to be a map and not just a plain old var?
     */
    private val liftedDefs =
      mutable.Map.empty[Symbol, mutable.ListBuffer[tpd.Tree]]

    override def transform(tree: tpd.Tree)(implicit ctx: Context) = {
      tree match {
        case PackageDef(pid, stats0) =>
          // Create buffer for nested defs
          val buf = new mutable.ListBuffer[tpd.Tree]
          liftedDefs(tree.symbol.moduleClass) = buf

          // Transform definitions. Nested classes will get lifted into buf
          val stats1 = transformStats(stats0)

          cpy.PackageDef(tree, transformSub(pid), stats1 ++ buf)
        case TypeDef(_, name, _) if tree.symbol.isNestedClass =>
          val sym = tree.symbol

          // todo: this insertion scheme (from Scalac 2.x) preserves ordering
          // in cases like:
          //
          //   `object O { trait A { trait B } }`
          //
          // where `B` should appear after `A` to allow Mixin to handle
          // accessors for private[this] trait fields. Is this still required
          // if Mixin is moved before erasure?

          val liftedBuffer = liftedDefs(sym.enclosingPackage)
          val index = liftedBuffer.length

          liftedBuffer.insert(index, super.transform(tree))

          tpd.EmptyTree

        // todo: Scalac 2.x contains another match here:
        //
        // case Template(...) if tree.symbol.isDefinedInPackage
        //  liftedDefs(tree.symbol.owner) = new ListBuffer
        //
        // is this required? why?
        case _ => super.transform(tree)
      }
    }

  }



}
