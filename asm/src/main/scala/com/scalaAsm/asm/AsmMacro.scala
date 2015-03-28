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
         
         val asmInstruction = (c.prefix.tree match {
            case Apply(_, List(Apply(_, xs))) => xs map {
              case Literal(Constant(x: String)) => x
            }
            case _ => Nil
          }).head
         
         
         
         //throw new Exception(what.toString)
        if (!asmInstruction.contains(' ')) {
          val mnemonic = asmInstruction
          c.Expr(Apply(Select(This(TypeName("$anon")), TermName(mnemonic)), List(Literal(Constant(())))))
        } else {
          val mnemonic = asmInstruction.split(' ').head
          val param = asmInstruction.split(' ').last
          if (param == "ebx") {
             c.Expr(Apply(Select(This(TypeName("$anon")), TermName(mnemonic)), List(Select(This(TypeName("HelloWorld")), TermName("ebx")))))
          } else {      
             c.Expr(Apply(Select(This(TypeName("$anon")), TermName(mnemonic)), List(Literal(Constant(param)))))
           }
        }
      }

}