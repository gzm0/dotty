package dotty.tools.dotc
package transform

import scala.collection.mutable

import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Types._
import core.Symbols._
import core.Scopes._
import core.Flags._
import ast.tpd._

import TreeTransforms._

import config.Printers.{ flatten => debug }

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
  def transform(ref: SingleDenotation)(implicit ctx: Context) = ctx.atPhase(this.id) { ctx0 =>
    debug.println(s"flattening: $ref")

    implicit val ctx: Context = ctx0

    val flatten = new TypeFlattener
    val info1 = flatten(ref.info)

    ref match {
      case sDet: SymDenotation =>
        val isLifted = !sDet.isTopLevel && (
            (sDet is Module) && !sDet.isStaticModule || sDet.isClass
        )

        val flags = if (isLifted) sDet.flags | Lifted else sDet.flags
        sDet.copySymDenotation(info = info1, initFlags = flags)
      case _ =>
        ref.derivedSingleDenotation(ref.symbol, info1)
    }
  }

  class TypeFlattener(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type): Type = tp match {
      case tp: TypeRef if !(tp.symbol.owner is PackageClass) =>
        debug.println(s"TypeRef $tp")

        // todo: 2.x checks here, whether the outer class doesn't have type
        // parameters so the lifted type doesn't reference them. But we are post
        // erasure so there should be no parameter types anymore
        // 2.x also doesn't handle type members
        TypeRef(tp.symbol.enclosingPackage.thisType, tp.name)
      case ClassInfo(prefix, cls, classParents, decls, selfInfo) =>
        debug.println(s"ClassInfo $tp")

        // Transform parent class info (nop for packages)
        val parents1 = classParents.mapConserve(this)

        // Transform declarations
        val decls1 = scopeTransform(cls) {
          val sc = newScope
          if (cls is PackageClass) {
            ctx.atNextPhase { ctx =>
              decls foreach { sc.enter(_)(ctx) }
            }
          } else {
            // todo: 2.x forces the info of the old owner here:
            //
            //   val oldowner = cls.owner
            //   exitingFlatten { oldowner.info }
            //
            // do we need this?

            for (sym <- decls) {
              if (sym.isTerm && !sym.isStaticModule || sym.isClass) {
                ctx.atNextPhase { ctx => sc.enter(sym)(ctx) }
              } else {
                debug.println(s"Other sym in class: $sym")
              }
            }
          }
          sc
        }

        debug.println(s"declarations $decls1")

        ClassInfo(prefix, cls, classParents, decls1, selfInfo)
      case _ =>
        mapOver(tp)
    }
  }

}
