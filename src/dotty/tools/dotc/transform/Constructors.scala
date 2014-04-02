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

class Constructors extends TreeTransform {

   def name = "constructors"
   override def description =
    """Creates the primary constructor from the class definition"""

   override def transformTemplate(tree: Template) = {

   }

   private class TemplateTransform {

     // The list of definitions that go into class
     private val defBuf = new ListBuffer[Tree]

     // The auxiliary constructors, separate from the defBuf since they should
     // follow the primary constructor
     private val auxConstructorBuf = new ListBuffer[Tree]

     // The list of statements that go into the constructor after and including the superclass constructor call
     private val constrStatBuf = new ListBuffer[Tree]

     // The early initialized field definitions of the class (these are the class members)
     private val presupers = treeInfo.preSuperFields(stats)

     // The list of statements that go into the class initializer
     private val classInitStatBuf = new ListBuffer[Tree]

   }

   //// Buffers for different parts of a template ////



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


}
