package dotty.tools.dotc
package transform

import scala.collection.mutable

import TreeTransforms._
import core.DenotTransformers._
import core.Denotations._
import core.Contexts._
import core.Types._
import core.Symbols._
import core.Flags._
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

  /** splices definitions back into parent package */
  override def transformPackageDef(tree: PackageDef)(
    implicit ctx: Context, info: TransformerInfo) = {

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

    // clear definition map for this package
    liftedDefs.remove(tree.symbol.moduleClass)

    cpy.PackageDef(tree, tree.pid, tree.stats ++ locBuf)
  }

  /** buffers nested classes for later splicing */
  override def transformTypeDef(tree: TypeDef)(
    implicit ctx: Context, info: TransformerInfo) = {

    val sym = tree.symbol

    if (sym.isNestedClass) {
      // Append the tree to the buffer, but don't transform it. We
      // want it to appear in the proper position for later
      // transforms. This also ensures that the following
      //
      //   `object O { trait A { trait B } }`
      //
      // will be correctly ordered (lifted `B` should appear after
      // lifted `A`).
      liftedDefs(sym.enclosingPackage).append(tree)

      EmptyTree
    } else tree
  }

  /** buffers nested static module vals for later splicing */
  override def transformValDef(tree: ValDef)(
    implicit ctx: Context, info: TransformerInfo) = {

    val sym = tree.symbol

    if (sym.isStaticModule && !sym.isTopLevel) {
      // todo: this splices these module accessors "relatively" late.
      // if we have:
      //
      //     `object A { object B }; object C { A.B }`
      //
      // B's accessor will be spliced after the definition of `C`. Is this an
      // issue?
      liftedDefs(sym.enclosingPackage).append(tree)
      EmptyTree
    } else tree

  }

  // todo: Scalac 2.x contains another match here:
  //
  // case Template(...) if tree.symbol.isDefinedInPackage
  //  liftedDefs(tree.symbol.owner) = new ListBuffer
  //
  // is this required? why?


  /** re-targets Select trees on static modules which we have lifted */
  override def transformSelect(tree: Select)(
    implicit ctx: Context, info: TransformerInfo) = {

    val sym = tree.symbol

    // Check if this select targets a static module whose accessor we have
    // lifted. If this is the case, we also need to change the Select tree.
    if (sym.isStaticModule && !sym.isTopLevel) {
      // todo
      tree
    } else tree
  }



  // todo
  def transform(ref: SingleDenotation)(implicit ctx: Context) = ref

  class TypeFlattener(implicit ctx: Context) extends TypeMap {
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
    }
  }

}
