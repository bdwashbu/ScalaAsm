package com.scalaAsm.asm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.scalaAsm.x86.InstructionResult
import scala.reflect.runtime.universe._
import scala.util.control.Exception.allCatch
import java.lang.Long;
import java.lang.Byte;

object AsmCompiler {

  val regList = Seq("ebx", "ebp", "eax", "ecx", "edx", "esp", "edi", "cl", "rsp", "rax", "spl", "rdx")

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

  def asmMacro(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

    val blah = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x
      }
      case _ => Nil
    })
    
    val asmInstruction = (if (blah.isEmpty) args(0).tree.symbol.name.decodedName.toString else blah.head).replaceAll("\\s+", " ").trim

    val params = Seq[Tree]((args map (_.tree)): _*)

    val result = if (!params.isEmpty) {
      x86Macro.x86(c)(args: _*)
    } else if (asmInstruction.contains(':')) { // label
      if (asmInstruction.endsWith(":") && asmInstruction.count(_ == ':') == 1 && !asmInstruction.contains(' ') && !asmInstruction.contains(',')) {
        val labelName = asmInstruction.reverse.tail.reverse
        s"""Label(\"$labelName\")"""
      } else {
        c.abort(c.enclosingPosition, s"Error: bad label format")
        ""
      }
    } else if (asmInstruction.contains(' ') && !asmInstruction.contains(',')) {
      val mnemonic = asmInstruction.split(' ').head.toUpperCase()
      val param = asmInstruction.split(' ').last
      if (regList.contains(param) || (param.contains("[") && param.contains("]"))) {
        x86Macro.x86(c)(args: _*)
      } else if (!isDword(param)) {
        val varName = param

        mnemonic match {
          case "CALL" => s"""FunctionReference(\"$varName\")"""
          case "PUSH" => s"""Reference(\"$varName\")"""
          case "JNZ" => s"""
                val ev = implicitly[JNZ#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef(\"$varName\", ev, format)
                """
          case "JZ" =>
               s"""
                val ev = implicitly[JZ#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef(\"$varName\", ev, format)
                """
          case "JE" =>
               s"""
                val ev = implicitly[JE#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef(\"$varName\", ev, format)
                """
          case "JMP" =>
               s"""
                val ev = implicitly[JMP#_1[Constant[_8]]]
                val format = implicitly[OneOperandFormat[Constant[_8]]]
                LabelRef(\"$varName\", ev, format)
                """
          case "INVOKE" => s"""Invoke(\"$varName\")"""
          case _        => x86Macro.x86(c)(args: _*)
        }
      } else {
        x86Macro.x86(c)(args: _*)
      }
    } else {
      x86Macro.x86(c)(args: _*)
    }
    
    c.Expr(c.parse(result))
  }

 

  

 

  

}