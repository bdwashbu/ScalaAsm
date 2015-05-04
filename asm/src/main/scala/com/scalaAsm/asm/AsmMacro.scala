package com.scalaAsm.asm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.scalaAsm.x86.InstructionResult
import com.scalaAsm.x86.Instructions.General._

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

import scala.reflect.runtime.universe._
import scala.util.control.Exception.allCatch
import java.lang.Long;
import java.lang.Byte;

object AsmMacro {

  val regList = Seq("ebx", "ebp", "eax", "ecx", "edx", "esp", "edi", "cl", "rsp", "rax", "spl")

  def isByte(s: String): Boolean = {
    if (s.contains("0x")) {
      Long.parseLong(s.drop(2), 16) < 256
    } else {
      (allCatch opt s.toByte).isDefined
    }
  }
  def isDword(s: String): Boolean = {
    if (s.contains("0x")) {
      (allCatch opt Long.parseLong(s.drop(2), 16)).isDefined
    } else {
      (allCatch opt s.toLong).isDefined
    }
  }

  def impl(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

    val asmInstruction = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x
      }
      case _ => Nil
    }).head
    
    val params = Seq[Tree]((args map (_.tree)): _*)

    if (!params.isEmpty) {
      impl2(c)(args: _*)
    } else if (asmInstruction.endsWith(":")) { // label
      val labelName = Constant(asmInstruction.reverse.tail.reverse)
      c.Expr(q"Label($labelName)")
    } else if (asmInstruction.contains(' ') && !asmInstruction.contains(',')) {
      val mnemonic = asmInstruction.split(' ').head.toUpperCase()
      val param = asmInstruction.split(' ').last
      if (regList contains param) {
        impl2(c)(args: _*)
      } else if (!isDword(param)) {
        val varName = Constant(param)
        
        mnemonic match {
          case "CALL" => c.Expr(q"FunctionReference($varName)")
          case "PUSH" => c.Expr(q"Reference($varName)")
          case "JNZ" => c.Expr(q"""
                val ev = implicitly[JNZ#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef($varName, ev, format)
                """)
          case "JZ" =>
            c.Expr(q"""
                val ev = implicitly[JZ#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef($varName, ev, format)
                """)
          case "JE" =>
            c.Expr(q"""
                val ev = implicitly[JE#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef($varName, ev, format)
                """)
          case "JMP" =>
            c.Expr(q"""
                val ev = implicitly[JMP#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef($varName, ev, format)
                """)
          case "INVOKE" => c.Expr(q"Invoke($varName)")
          case _ => impl2(c)(args: _*)
        }
      } else {
        impl2(c)(args: _*)
      }
    } else {
      impl2(c)(args: _*)
    }
  }

  def impl2(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

    val parts = c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        rawParts zip (args map (_.tree)) map {
          case (Literal(Constant(rawPart: String)), arg) =>
            arg.toString
        }
    }

    val toolBox = currentMirror.mkToolBox()
    val importer = c.universe.mkImporter(ru)

    val asmInstructions = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x
      }
      case _ => Nil
    })
    
    val asmInstruction = asmInstructions.head
    
    val fullInst = asmInstructions.reduce(_ + "" + _)
    

    //         if (args.size > 0) {
    //           throw new Exception(c.internal.enclosingOwner.asClass.fullName)
    //         }

    // throw new Exception(c.internal.enclosingOwner.asClass.fullName)
    val params = Seq[Tree]((args map (_.tree)): _*)
    //throw new Exception(asmInstructions.reduce(_ + ", " + _))
    if (!params.isEmpty) {
      val mnemonic = TermName(asmInstruction.split(' ').head.toUpperCase)
      //throw new Exception(params.head.toString())
      if (params.head.toString contains "toString") {
        val param = TermName(asmInstruction.split(' ').tail.mkString.split(',').head)
       // throw new Exception("kookoo")
        c.Expr(q"$mnemonic($param, dword(input))")
      } else if (!fullInst.contains(',')) {
        //throw new Exception("fuckcck")
        val x = q"${args(0)}"
        if (isByte(x.toString)) {
          c.Expr(q"$mnemonic(byte($x.toByte))")
        } else {
          c.Expr(q"$mnemonic($x)")
        }
      } else if (asmInstructions.size == 2 && asmInstructions.last.size == 0) { // interpolated var at end of string
        val param = TermName(asmInstruction.split(' ').tail.mkString.split(',').head)
        val paramString = parts(0).split('.').last
        val param2 = TermName(paramString)
        //throw new Exception(params(0).tpe.typeSymbol.name)
        //val x = q"${args(0)}"
        //throw new Exception(params(0).tpe.typeSymbol.name.toString)
        //c.Expr(q"$mnemonic($param, dword($param2.toInt))")
        params(0).tpe.typeSymbol.name.toString match {
          case "Byte" => c.Expr(q"$mnemonic($param, byte($param2))")
          case "Long" | "Int" => c.Expr(q"$mnemonic($param, dword($param2))")
          case _ => c.Expr(q"$mnemonic($param, $param2)")
        }
      } else {
        val param = TermName(asmInstructions.last.split(' ').tail.mkString.split(',').head)
        val x = q"${args(0)}"
        c.Expr(q"$mnemonic($x, $param)")
      }
    } else if (asmInstruction.charAt(0) == '[') {
      val times = Select(This(typeNames.EMPTY), TermName("$times"))
      val ebpReg = Select(This(typeNames.EMPTY), TermName("ebp"))
      val byteTerm = Select(This(typeNames.EMPTY), TermName("byte"))
      val conforms = Select(Ident(TermName("scala.Predef")), TermName("$conforms"))
      c.Expr(Apply(times, List(Apply(Select(ebpReg, TermName("$plus")), List(Apply(byteTerm, List(Literal(Constant(-4)))))))))
    } else if (!asmInstruction.contains(' ')) {
      val mnemonic = TermName(asmInstruction.toUpperCase())
      c.Expr(q"$mnemonic(())")
    } else if (asmInstruction.contains(' ') && !asmInstruction.contains(',')) { // one operand
      val mnemonic = TermName(asmInstruction.split(' ').head.toUpperCase())
      val param = asmInstruction.split(' ').last
      if (regList contains param) {
        val term1 = TermName(param)
        c.Expr(q"$mnemonic($term1)")
      } else {  
        val term1 = Constant(param)
        if (isDword(param)) {
          c.Expr(q"$mnemonic(dword($term1.toInt))")
        } else {
          c.Expr(q"$mnemonic($term1)")
        }
      }
    } else { // two operands
      val mnemonic = TermName(asmInstruction.split(' ').head.toUpperCase())
      val params = asmInstruction.split(' ').tail.reduce(_ + " " + _).split(',').map { param =>
        if (param.contains("(")) {
          param.trim.split("(").last.split(")").head
        } else {
          param.trim
        }
      }

      if ((regList contains params(0)) && (regList contains params(1))) {
        val term1 = TermName(params(0))
        val term2 = TermName(params(1))
        c.Expr(q"$mnemonic($term1, $term2)")
      } else if ((regList contains params(0)) && params(1).split(" ").head == "byte") {
        //throw new Exception("FFFFFFFFFFFFF")
        val term1 = TermName(params(0))
        val constant = Constant(params(1).split(" ").last)
        c.Expr(q"$mnemonic($term1, byte($constant.toByte))")
      } else if ((regList contains params(0)) && params(1).split(" ").head == "dword") {
        //throw new Exception("FFFFFFFFFFFFF")
        val term1 = TermName(params(0))
        val constant = Constant(params(1).split(" ").last)
        c.Expr(q"$mnemonic($term1, dword($constant.toInt))")
      } else if ((regList contains params(0)) && isByte(params(1))) {
        
        val term1 = TermName(params(0))
        if (params(1).contains("0x")) {
          val constant = Constant(Integer.parseInt(params(1).drop(2), 16))
          c.Expr(q"$mnemonic($term1, byte($constant.toByte))")
        } else {
          val constant = Constant(params(1))
          c.Expr(q"$mnemonic($term1, byte($constant.toByte))")
        }
      } else if ((regList contains params(0)) && isDword(params(1))) {
        //throw new Exception(params.reduce(_ + ", " + _))
        
        val term1 = TermName(params(0))
        if (params(1).contains("0x")) {
          val constant = Constant(Long.parseLong(params(1).drop(2), 16))
          c.Expr(q"$mnemonic($term1, dword($constant.toInt))")
        } else {
          val constant = Constant(params(1))
          c.Expr(q"$mnemonic($term1, dword($constant.toInt))")
        }
      } else {
        c.Expr(Apply(Select(This(TypeName("$anon")), mnemonic), List(Literal(Constant(())))))
      }

      //Expr(Apply(Select(Select(Ident(TermName("$anon")), TermName("mov")), TermName("apply")), List(Select(Ident(TermName("$anon")), TermName("ebp")), Apply(Select(Ident(TermName("$anon")), TermName("esp")))))))

    }
  }

}