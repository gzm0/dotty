package dotty.tools.dotc
package transform

import scala.collection.mutable

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.Contexts._
import core.Types._
import core.Symbols._
import ast.tpd._

class Flatten extends TreeTransform with DenotTransformer {

  def name = "flatten"
  override def description =
    """The flatten phase lifts nested classes to the package level"""

  /** Intermediate store for class definitions that are about to be lifted
    *
    *  Use a map from package module symbols to the buffers containing
    *  TypeDef trees that are going to be lifted. This is a map (rather than a
    *  var) in order to handle nested packages correctly.
    */
  private val liftedDefs =
    mutable.Map.empty[Symbol, mutable.ListBuffer[Tree]]

  override def prepareForPackageDef(tree: PackageDef)(implicit ctx: Context) = {
    // Create buffer for nested defs
    val buf = new mutable.ListBuffer[Tree]
    liftedDefs(tree.symbol.moduleClass) = buf
    this
  }

  override def transformPackageDef(tree: PackageDef)(
    implicit ctx: Context, info: TransformerInfo) = {

    // todo: am I responsible for this??
    //val stats1 = transformStats(tree.stats0)

    // By this point, nested classses are lifted in the corresponding
    // buffer, but we still have to transform these trees. This can
    // result in other lifted classes. We therefore iterate until we
    // find a fixpoint.
    val globBuf = liftedDefs(tree.symbol.moduleClass)
    val locBuf  = new mutable.ListBuffer[Tree]
    
    while (globBuf.nonEmpty) {
      val curElems = globBuf.toList
      globBuf.clear()
      locBuf ++= transformStats(curElems)
    }

    // todo: do I need to transform pid here?
    cpy.PackageDef(tree, tree.pid, tree.stats ++ locBuf)
  }

  override def transformTypeDef(tree: TypeDef)(
    implicit ctx: Context, info: TransformerInfo) = {

    val sym = tree.symbol

    if (sym.isNestedClass) {
      // Buffer for these trees
      val liftedBuffer = liftedDefs(sym.enclosingPackage)

      // Append the tree to the buffer, but don't transform it. We
      // want it to appear in the proper position for later
      // transforms. This also ensures that the following
      //
      //   `object O { trait A { trait B } }`
      //
      // will be correctly ordered (lifted `B` should appear after
      // lifted `A`). 
      liftedBuffer.append(tree)

      EmptyTree
    } else tree
  }

  // todo: Scalac 2.x contains another match here:
  //
  // case Template(...) if tree.symbol.isDefinedInPackage
  //  liftedDefs(tree.symbol.owner) = new ListBuffer
  //
  // is this required? why?


  override def transformSelect(tree: Select)(
    implicit ctx: Context, info: TransformerInfo) = {

    // todo
    // case Select(qual, name) if sym.isStatic && sym.is(Flags.Module) && !sym.isTopLevel =>
    tree
  }

  // todo
  def transform(ref: SingleDenotation)(implicit ctx: Context) = ref

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

}
