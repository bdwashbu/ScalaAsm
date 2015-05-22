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

    val text = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x
      }
      case _ => Nil
    })
    
    def interleave(xs: Seq[Object], ys: Seq[Object], n: Int): Seq[Object] = {
      val iter = xs grouped n
      val coll = iter zip ys.iterator flatMap { case (xs, y) => if (xs.size == n) xs :+ y else xs }
      (coll ++ iter.flatten).toIndexedSeq
    }
    
    val interleved = interleave(text, args, 1)
    val withSeparators: Seq[Any] = interleved.flatMap{x => x match {
      case x: String => x.split("\n").head +: x.split("\n").tail.map{x => List(0, x)}
      case x: c.Expr[_] => List(x)
    }}
    
    //c.abort(c.enclosingPosition, splitBySeparator(withSeparators, 0).toString)
    
    def splitBySeparator( l: Seq[Any], sep: Any ): Seq[Seq[Any]] = {
      import collection.mutable.ListBuffer
      val b = ListBuffer(ListBuffer[Any]())
      l foreach { e =>
        if ( e == sep ) {
          if  ( !b.last.isEmpty ) b += ListBuffer[Any]()
        }
        else b.last += e
      }
      b.map(_.toSeq)
    }
    
    val translated = splitBySeparator(withSeparators, 0).map { line =>
      
      val newArgs = line.collect{case x:c.Expr[_] => x}
      val inst = line.collect{case x:String => x}.toList
    
      val asmInstruction = (if (inst.isEmpty) newArgs(0).tree.symbol.name.decodedName.toString else inst.head).replaceAll("\\s+", " ").trim
  
      val params = Seq[Tree]((newArgs map (_.tree)): _*)
  
      if (!params.isEmpty) {
        x86Macro.x86(c, inst)(args: _*)
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
          x86Macro.x86(c, inst)(args: _*)
        } else if (!isDword(param)) {
          val varName = param
  
          mnemonic match {
            case "CALL" =>
              s"""FunctionReference(\"$varName\")"""
            case "PUSH" =>
              s"""Reference(\"$varName\")"""
            case "JNZ" =>
              s"""LabelRef(\"$varName\", implicitly[JNZ#_1[Constant[_8]]], implicitly[OneOperandFormat[Constant[_8]]])"""
            case "JZ" =>
              s"""LabelRef(\"$varName\", implicitly[JZ#_1[Constant[_8]]], implicitly[OneOperandFormat[Constant[_8]]])"""
            case "JE" =>
              s"""LabelRef(\"$varName\", implicitly[JE#_1[Constant[_8]]], implicitly[OneOperandFormat[Constant[_8]]])"""
            case "JMP" =>
              s"""LabelRef(\"$varName\", implicitly[JMP#_1[Constant[_8]]], implicitly[OneOperandFormat[Constant[_8]]])"""
            case "INVOKE" => s"""Invoke(\"$varName\")"""
            case _        => x86Macro.x86(c, inst)(args: _*)
          }
        } else {
          x86Macro.x86(c, inst)(args: _*)
        }
      } else {
        x86Macro.x86(c, inst)(args: _*)
      }
    }
    
    c.Expr(c.parse(translated(0)))
  }

 

  

 

  

}