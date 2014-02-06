package dotty.tools.dotc
package transform

import scala.collection.mutable

import core._
import Phases._
import Contexts._
import Symbols._
import parsing.Parsers.Parser
import config.Printers._
import Denotations._

class Flatten extends Phase {

  import ast.tpd
  import ast.Trees._

  // todo: core.Phases.PhasesBase also defines "flatten"
  def name = "flatten"

  override def description =
    """The flatten phase lifts nested classes to the package level"""

  override def run(implicit ctx: Context): Unit = {

    // todo: is this a sym or a ref transformer?
    class FlattenSymTransformer extends ctx.base.symTransformers.Transformer {
      // todo: is this correct? transform post flatten
      val phaseId: Int = Flatten.this.next.id

      /** The transformation method */
      def transform(ref: SingleDenotation)(implicit ctx: Context) = {
        println(s"asked to transform $ref")

        /*
         * This is stuff copied from 2.x
         *
          def apply(tp: Type): Type = tp match {
      case TypeRef(pre, sym, args) if isFlattenablePrefix(pre) =>
        assert(args.isEmpty && sym.enclosingTopLevelClass != NoSymbol, sym.ownerChain)
        typeRef(sym.enclosingTopLevelClass.owner.thisType, sym, Nil)
      case ClassInfoType(parents, decls, clazz) =>
        var parents1 = parents
        val decls1 = scopeTransform(clazz) {
          val decls1 = newScope
          if (clazz.isPackageClass) {
            exitingFlatten { decls foreach (decls1 enter _) }
          }
          else {
            val oldowner = clazz.owner
            exitingFlatten { oldowner.info }
            parents1 = parents mapConserve (this)

            for (sym <- decls) {
              if (sym.isTerm && !sym.isStaticModule) {
                decls1 enter sym
                if (sym.isModule)
                  sym.moduleClass setFlag LIFTED
              } else if (sym.isClass)
                liftSymbol(sym)
            }
          }
          decls1
        }
        ClassInfoType(parents1, decls1, clazz)
      case MethodType(params, restp) =>
        val restp1 = apply(restp)
        if (restp1 eq restp) tp else copyMethodType(tp, params, restp1)
      case PolyType(tparams, restp) =>
        val restp1 = apply(restp)
        if (restp1 eq restp) tp else PolyType(tparams, restp1)
      case _ =>
        mapOver(tp)
    } */
ref
      }

    }

    // todo: phase id
    val symTrans = new FlattenSymTransformer()
    ctx.base.symTransformers.install(symTrans.phaseId, symTrans)

    val flatter = new TreeFlattener
    val oldTree = ctx.compilationUnit.tpdTree
    val newTree = flatter.transform(oldTree)
    println("Tree after flatten")
    println(newTree.show)
    ctx.compilationUnit.tpdTree = newTree

  }

  /** TreeFlattener moves nested class definitions to the package level */
  class TreeFlattener extends tpd.TreeTransformer {

    /** Intermediate store for class definitions that are about to be lifted
     *
     *  Use a map from package module symbols to the buffers containing
     *  TypeDef trees that are going to be lifted. This is a map (rather than a
     *  var) in order to handle nested packages correctly.
     */
    private val liftedDefs =
      mutable.Map.empty[Symbol, mutable.ListBuffer[tpd.Tree]]

    override def transform(tree: tpd.Tree)(implicit ctx: Context) =
      postTransform(preTransform(tree))

    private def preTransform(tree: tpd.Tree)(implicit ctx: Context) = {
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

    private def postTransform(tree: tpd.Tree)(implicit ctx: Context) = {
      lazy val sym = tree.symbol
      tree match {
        case Select(qual, name) if sym.isStatic && sym.is(Flags.Module) && !sym.isTopLevel =>
        case _ =>
      }
      tree
    }

  }



}
