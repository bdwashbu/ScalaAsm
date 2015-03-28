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
         
         val mnemonic = c.prefix.tree match {
            case Apply(_, List(Apply(_, xs))) => xs map {
              case Literal(Constant(x: String)) => x.split(' ').head
            }
            case _ => Nil
          }
         
         //throw new Exception(what.toString)
         
        if (what.head == "ebx") {
           //c.Expr(Apply(Select(Select(This(TypeName("HelloWorld")), TermName("fuck")), TermName("pop")), List(Apply(Select(This(TypeName("HelloWorld")), TermName("ebx")), List(Select(Ident("scala.Predef"), TermName("$conforms")))))))
           //c.Expr(Apply(Apply(Select(Select(Ident(TermName("$anon")), TermName("pop")), TermName("apply")), List(Apply(Select(This(TypeName("HelloWorld")), TermName("ebx")), List(Select(Ident(TermName("scala.Predef")), TermName("$conforms")))))), List(Select(Ident(TermName("com.scalaAsm.x86.Instructions.General.POP")), TermName("POP_5")), Select(This(TypeName("HelloWorld")), TermName("New_MFormat4")))))
           c.Expr(Apply(Select(This(TypeName("$anon")), TermName("pop")), List(Select(This(TypeName("HelloWorld")), TermName("ebx")))))
        } else {      
           c.Expr(Apply(Select(This(TypeName("$anon")), TermName(mnemonic.head)), List(Literal(Constant(what.head)))))
         }
      }

}