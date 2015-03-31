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

import scala.reflect.runtime.universe._
import scala.util.control.Exception.allCatch

object AsmMacro {
  
      def isByte(s: String): Boolean = (allCatch opt s.toByte).isDefined
  
      def impl (c : Context) (args: c.Expr[String]*): c.Expr[Function0[Tokens.Reference]] = {
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
         
         
         
        // throw new Exception(c.internal.enclosingOwner.asClass.fullName)
        if (!args.isEmpty) {
          val mnemonic = TermName(asmInstruction.split(' ').head)
          val x = q"${args(0)}"
          c.Expr(q"$mnemonic(byte($x.toByte))")
        } else if (asmInstruction.charAt(0) == '[') {
          val times = Select(This(typeNames.EMPTY), TermName("$times"))
          val ebpReg = Select(This(typeNames.EMPTY), TermName("ebp"))
          val byteTerm = Select(This(typeNames.EMPTY), TermName("byte"))
          val conforms = Select(Ident(TermName("scala.Predef")), TermName("$conforms"))
          c.Expr(Apply(times, List(Apply(Select(ebpReg, TermName("$plus")), List(Apply(byteTerm, List(Literal(Constant(-4)))))))))
        } else if (!asmInstruction.contains(' ')) {
          val mnemonic = asmInstruction
          c.Expr(Apply(Select(This(TypeName("$anon")), TermName(mnemonic)), List(Literal(Constant(())))))
        } else if (asmInstruction.contains(' ') && !asmInstruction.contains(',')){
          val mnemonic = asmInstruction.split(' ').head
          val param = asmInstruction.split(' ').last
          if (Seq("ebx", "ebp", "eax", "ecx", "edx") contains param) {
             c.Expr(Apply(Select(This(typeNames.EMPTY), TermName(mnemonic)), List(Select(This(typeNames.EMPTY), TermName(param)))))
          } else {      
             c.Expr(Apply(Select(This(typeNames.EMPTY), TermName(mnemonic)), List(Literal(Constant(param)))))
          }
        } else {
          val mnemonic = TermName(asmInstruction.split(' ').head)
          val params = asmInstruction.split(' ').tail.mkString.split(',').map{ param =>
            if (param.contains("(")) {
              param.split("(").last.split(")").head
            } else {
              param
            }
          }
          
           

          if ((Seq("ebx", "ebp", "eax", "ecx", "edx", "esp") contains params(0)) && (Seq("ebx", "ebp", "eax", "ecx", "edx", "esp") contains params(1))) {
            val term1 = TermName(params(0))
            val term2 = TermName(params(1))
            c.Expr(q"$mnemonic($term1, $term2)")
          } else if ((Seq("ebx", "ebp", "eax", "ecx", "edx", "esp") contains params(0)) && isByte(params(1))) {
            //throw new Exception(params.reduce(_ + ", " + _))
            val term1 = TermName(params(0))
            val aByte = TermName("byte") 
            val constant = Constant(params(1))
            c.Expr(q"$mnemonic($term1, byte($constant.toByte))")
          } 
          else {
            c.Expr(Apply(Select(This(TypeName("$anon")), mnemonic), List(Literal(Constant(())))))
          }

          //Expr(Apply(Select(Select(Ident(TermName("$anon")), TermName("mov")), TermName("apply")), List(Select(Ident(TermName("$anon")), TermName("ebp")), Apply(Select(Ident(TermName("$anon")), TermName("esp")))))))
          
        }
      }
      
      def typeImpl (c : Context) (): c.Expr[Operand[_]] = {
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
         
         
         
        // throw new Exception(c.internal.enclosingOwner.asClass.fullName)
        //if (asmInstruction.charAt(0) == '[') {
          val times = Select(This(typeNames.EMPTY), TermName("$times"))
          val ebpReg = Select(This(typeNames.EMPTY), TermName("ebp"))
          val byteTerm = Select(This(typeNames.EMPTY), TermName("byte"))
          val conforms = Select(Ident(TermName("scala.Predef")), TermName("$conforms"))
          c.Expr(Apply(times, List(Apply(Select(ebpReg, TermName("$plus")), List(Apply(byteTerm, List(Literal(Constant(-4)))))))))
        //}
      }

}