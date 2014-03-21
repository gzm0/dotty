package dotty.tools.dotc
package transform

import scala.collection.mutable

import core.DenotTransformers._
import core.Denotations._
import core.Contexts._
import core.Types._
import core.Symbols._
import core.Flags._
import ast.tpd._

import TreeTransforms._

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
    liftedDefs(tree.symbol.moduleClass) = new mutable.ListBuffer[Tree]
    this
  }

  /** splices definitions back into parent package */
  override def transformPackageDef(tree: PackageDef)(
    implicit ctx: Context, info: TransformerInfo) = {

    // Splice nested classes. Call transformFollowing to apply transforms of
    // subsequent phases.
    val innerStats = liftedDefs.remove(tree.symbol.moduleClass).get
    val transStats = innerStats.map(transformFollowing _)

    cpy.PackageDef(tree, tree.pid, tree.stats ++ transStats)
  }

  /** buffers nested classes for later splicing */
  override def transformTypeDef(tree: TypeDef)(
    implicit ctx: Context, info: TransformerInfo) = {

    val sym = tree.symbol

    if (sym.isNestedClass) {
      // Append the tree to the buffer, but don't transform it. We
      // want it to appear in the proper position for later
      // transforms.

      // todo: Flatten in 2.x ensures the that the following
      //
      //   `object O { trait A { trait B } }`
      //
      // will be correctly ordered (lifted `B` should appear after
      // lifted `A`). We don't do that currently. Is it required?
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
      case TypeRef(prefix, name) if isFlattenablePrefix(pre) =>
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
