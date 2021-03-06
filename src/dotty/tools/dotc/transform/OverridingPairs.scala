package dotty.tools.dotc
package transform

import core._
import Flags._, Symbols._, Contexts._, Types._, Scopes._
import util.HashSet
import collection.mutable.HashMap
import collection.immutable.BitSet
import scala.annotation.tailrec

/** A class that yields a kind of iterator (`Cursor`),
 *  which yields all pairs of overriding/overridden symbols
 *  that are visible in some baseclass, unless there's a parent class
 *  that already contains the same pairs.
 *
 *  Adapted from the 2.9 version of OverridingPairs. The 2.10 version is IMO
 *  way too unwieldy to be maintained.
 */
abstract class OverridingPairs {

  /** The cursor class
   *  @param base   the base class that contains the overriding pairs
   */
  class Cursor(base: Symbol)(implicit ctx: Context) {

    private val self = base.thisType

    /** Symbols to exclude: Here these are constructors and private locals.
     *  But it may be refined in subclasses.
     */
    protected def exclude(sym: Symbol): Boolean =
      sym.isConstructor || sym.is(PrivateLocal)

    /** The parents of base (may also be refined).
     */
    protected def parents: Array[Symbol] = base.info.parents.toArray map (_.typeSymbol)

    /** Does `sym1` match `sym2` so that it qualifies as overriding.
     *  Types always match. Term symbols match if their membertypes
     *  relative to <base>.this do
     */
    protected def matches(sym1: Symbol, sym2: Symbol): Boolean =
      sym1.isType || {
        val info1 = self.memberInfo(sym1)
        val info2 = self.memberInfo(sym2)
        // info1.signature == info2.signature &&  // TODO enable for speed
        info1 matches info2
      }

    /** The symbols that can take part in an overriding pair */
    private val decls = {
      val decls = newScope
      // fill `decls` with overriding shadowing overridden */
      def fillDecls(bcs: List[Symbol], deferred: Boolean): Unit = bcs match {
        case bc :: bcs1 =>
          fillDecls(bcs1, deferred)
          var e = bc.info.decls.asInstanceOf[MutableScope].lastEntry
          while (e != null) {
            if (e.sym.is(Deferred) == deferred && !exclude(e.sym))
              decls.enter(e.sym)
            e = e.prev
          }
        case nil =>
      }
      // first, deferred (this will need to change if we change lookup rules!
      fillDecls(base.info.baseClasses, deferred = true)
      // then, concrete.
      fillDecls(base.info.baseClasses, deferred = false)
      decls
    }

    private val subParents = {
      val subParents = new HashMap[Symbol, BitSet]
      for (bc <- base.info.baseClasses)
        subParents(bc) = BitSet(parents.indices.filter(parents(_).derivesFrom(bc)): _*)
      subParents
    }

    private def hasCommonParentAsSubclass(cls1: Symbol, cls2: Symbol): Boolean =
      (subParents(cls1) intersect subParents(cls2)).isEmpty

    /** The scope entries that have already been visited as overridden
     *  (maybe excluded because of hasCommonParentAsSubclass).
     *  These will not appear as overriding
     */
    private val visited = new HashSet[ScopeEntry](64)

    /** The current entry candidate for overriding
     */
    private var curEntry = decls.lastEntry

    /** The current entry candidate for overridden */
    private var nextEntry = curEntry

    /** The current candidate symbol for overriding */
    var overriding: Symbol = _

    /** If not null: The symbol overridden by overriding */
    var overridden: Symbol = _

    //@M: note that next is called once during object initialization
    def hasNext: Boolean = curEntry ne null

    @tailrec
    final def next: Unit = {
      if (curEntry ne null) {
        overriding = curEntry.sym
        if (nextEntry ne null) {
          val overridingOwner = overriding.owner
          do {
            do {
              nextEntry = decls.lookupNextEntry(nextEntry);
              /* DEBUG
              if ((nextEntry ne null) &&
                  !(nextEntry.sym hasFlag PRIVATE) &&
                  !(overriding.owner == nextEntry.sym.owner) &&
                  !matches(overriding, nextEntry.sym))
                println("skipping "+overriding+":"+self.memberType(overriding)+overriding.locationString+" to "+nextEntry.sym+":"+self.memberType(nextEntry.sym)+nextEntry.sym.locationString)
              */
              } while ((nextEntry ne null) &&
                       (//!!!!nextEntry.sym.canMatchInheritedSymbols ||
                        (overriding.owner == nextEntry.sym.owner) ||
                        (!matches(overriding, nextEntry.sym)) ||
                        (exclude(overriding))))
            if (nextEntry ne null) visited.addEntry(nextEntry)
            // skip nextEntry if a class in `parents` is a subclass of the owners of both
            // overriding and nextEntry.sym
          } while ((nextEntry ne null) &&
                   hasCommonParentAsSubclass(overridingOwner, nextEntry.sym.owner))
          if (nextEntry ne null) {
            overridden = nextEntry.sym;
            //Console.println("yield: " + overriding + overriding.locationString + " / " + overridden + overridden.locationString);//DEBUG
          } else {
            do {
              curEntry = curEntry.prev
            } while ((curEntry ne null) && visited.contains(curEntry))
            nextEntry = curEntry
            next
          }
        }
      }
    }

    next
  }
}
