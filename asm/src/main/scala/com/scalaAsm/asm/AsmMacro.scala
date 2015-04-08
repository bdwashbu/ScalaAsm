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
      def isDword(s: String): Boolean = (allCatch opt s.toLong).isDefined
  
      def impl (c : Context) (args: c.Expr[Any]*): c.Expr[Function0[Tokens.Reference]] = {
        import c.universe._
        import scala.reflect.runtime.{currentMirror => cm}
        import scala.reflect.runtime.{universe => ru}
        
        def toAST[A : TypeTag](xs: Tree*): Tree =
          Apply(
            Select(Ident(typeOf[A].typeSymbol.companionSymbol), newTermName("apply")),
            xs.toList)
    
        val parts = c.prefix.tree match {
          case Apply(_, List(Apply(_, rawParts))) =>
            rawParts zip (args map (_.tree)) map {
              case (Literal(Constant(rawPart: String)), arg) =>
                arg.toString
            }
        }

        val toolBox = currentMirror.mkToolBox()
        val importer = c.universe.mkImporter(ru)
         
        val asmInstruction = (c.prefix.tree match {
           case Apply(_, List(Apply(_, xs))) => xs map {
             case Literal(Constant(x: String)) => x
           }
           case _ => Nil
        }).head
         
//         if (args.size > 0) {
//           throw new Exception(c.internal.enclosingOwner.asClass.fullName)
//         }
         
        // throw new Exception(c.internal.enclosingOwner.asClass.fullName)
        val params = Seq[Tree]((args map (_.tree)): _*)
        
        if (!params.isEmpty) {
          val mnemonic = TermName(asmInstruction.split(' ').head)
          
          if (params.head.toString contains "toString") {
            val param = TermName(asmInstruction.split(' ').tail.mkString.split(',').head)
            
            c.Expr(q"$mnemonic($param, dword(input))")
          } else if (!asmInstruction.contains(',')) {
            //throw new Exception("fuckcck")
            val x = q"${args(0)}"
            if (isByte(x.toString)) {
              c.Expr(q"$mnemonic(byte($x.toByte))")
            } else {
              c.Expr(q"$mnemonic($x)")
            }
          } else {          
            val param = TermName(asmInstruction.split(' ').tail.mkString.split(',').head)
            val param2 = TermName(parts(0).split('.').last)
            val x = q"${args(0)}"
            //throw new Exception(parts(0).toString)
            c.Expr(q"$mnemonic($param, dword($param2.toInt))")
//            if (isByte(x.toString)) {
//              throw new Exception("1")
//              c.Expr(q"$mnemonic($param, byte($x.toByte))")
//            } else if (isDword(x.toString)) {
//              throw new Exception("2")
//              c.Expr(q"$mnemonic($param, dword($x.toLong))")
//            } else {
//              throw new Exception("3")
//              c.Expr(q"$mnemonic($param, $x)")
//            }
          }
        } else if (asmInstruction.charAt(0) == '[') {
          val times = Select(This(typeNames.EMPTY), TermName("$times"))
          val ebpReg = Select(This(typeNames.EMPTY), TermName("ebp"))
          val byteTerm = Select(This(typeNames.EMPTY), TermName("byte"))
          val conforms = Select(Ident(TermName("scala.Predef")), TermName("$conforms"))
          c.Expr(Apply(times, List(Apply(Select(ebpReg, TermName("$plus")), List(Apply(byteTerm, List(Literal(Constant(-4)))))))))
        } else if (asmInstruction.endsWith(":")) { // label
          val labelName = Constant(asmInstruction.reverse.tail.reverse)
          c.Expr(q"() => Label($labelName)") 
        } else if (!asmInstruction.contains(' ')) {
          val mnemonic = asmInstruction
          c.Expr(Apply(Select(This(TypeName("$anon")), TermName(mnemonic)), List(Literal(Constant(())))))
        } else if (asmInstruction.contains(' ') && !asmInstruction.contains(',')){
          val mnemonic = asmInstruction.split(' ').head
          val param = asmInstruction.split(' ').last
          if (Seq("ebx", "ebp", "eax", "ecx", "edx") contains param) {
             c.Expr(Apply(Select(This(typeNames.EMPTY), TermName(mnemonic)), List(Select(This(typeNames.EMPTY), TermName(param)))))
          } else if (isDword(param) ){      
             c.Expr(Apply(Select(This(typeNames.EMPTY), TermName(mnemonic)), List(Literal(Constant(param)))))
          } else if (mnemonic == "push") {   
            val varName = Constant(param)
            c.Expr(q"() => Reference($varName)")
          } else if (mnemonic == "jnz") {   
            val varName = Constant(param)
            c.Expr(q"""
              val ev = implicitly[JNZ#_1[Constant8]]
              val format = implicitly[OneOperandFormat[Constant8]]
              () => LabelRef($varName, ev, format)
              """)
          } else if (mnemonic == "jz") {   
            val varName = Constant(param)
              c.Expr(q"""
              val ev = implicitly[JZ#_1[Constant8]]
              val format = implicitly[OneOperandFormat[Constant8]]
              () => LabelRef($varName, ev, format)
              """)
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