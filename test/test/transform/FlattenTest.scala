package test.transform

import test.DottyTest
import org.junit.Test
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.Flags

class FlattenTest extends DottyTest {

  @Test
  def symbolPrint = checkCompile("flatten",
    """
    // Inner and Local classes
    class A {
      class InnerA {
        class InnerAInner
      }

      def method = {
        class LocalClass
      }

      object InnerB

      type Foo
    }

    // Static objects
    object B {
      object StaticObject
    }

    object User {
      println(B.StaticObject)
    }

    // Nexted packages
    package hello {
      class Foo {
        class Bar
      }
    }
    """
  ) { (tree, context) =>

    implicit val ctx = context

    println("Flatten done. Tree:")

    println(tree.show)

    println("Syms")
    printSymHier(tree.symbol, 0)

  }

  def printSymHier(sym: Symbol, ident: Int)(implicit ctx: Context): Unit = {
    println(s"${"  " * ident}${sym.denot}")
    for {
      d <- sym.info.decls if !d.is(Flags.Method)
    } printSymHier(d, ident + 1)
  }

}
