package com.scalaAsm.asm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions.General._

import com.scalaAsm.x86.Instructions.Catalog
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

object AsmMacro {
  
      def impl (c : Context) (): c.Expr[Function0[Tokens.Reference]] = {
         import c.universe._
         import scala.reflect.runtime.{currentMirror => cm}
         import scala.reflect.runtime.{universe => ru}
         val toolBox = currentMirror.mkToolBox()
         val importer = c.universe.mkImporter(ru)
         
         val what = c.prefix.tree match {
            case Apply(_, List(Apply(_, xs))) => xs map {
              case Literal(Constant(x: String)) => x.split(' ').last
            }
            case _ => Nil
          }
         
         //throw new Exception("AssertionError: " + what.head + ":"  + showRaw(c.prefix.tree))
//         val name = x.tree match {
//            case Literal(Constant(text: String)) => text.split('(').last.split(')').head
//            case _ => ""
//         }
         

         
         
         //val name = x.value.split('(').last.split(')').head
         //val tree = importer.importTree(toolBox.parse(code))
         //c.Expr(Apply(
         //   Ident(TermName("println")),
         //   List(Literal(Constant("hello")))))
            
         c.Expr(Apply(Select(Select(This(TypeName("HelloWorld")), TermName("fuck")), TermName("push")), List(Literal(Constant(what.head)))))
      }

}