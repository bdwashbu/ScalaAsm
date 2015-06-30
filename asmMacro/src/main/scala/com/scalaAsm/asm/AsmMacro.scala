package com.scalaAsm.asm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import com.scalaAsm.x86.InstructionResult
import scala.reflect.runtime.universe._
import scala.util.control.Exception.allCatch
import java.lang.Long
import java.lang.Byte
import java.math.BigInteger

object AsmCompiler {

  val regList = Seq("ebx", "ebp", "eax", "ecx", "edx", "esp", "edi", "esi", "cl", "bl", "rsp", "rax", "spl", "rdx")

  def isByte(s: String): Boolean = {
    if (s.contains("0x")) {
      Long.parseLong(s.drop(2), 16) < 256
    } else {
      (allCatch opt s.toByte).isDefined
    }
  }
  def isWord(s: String): Boolean = {
    if (s.contains("0x")) {
      Long.parseLong(s.drop(2), 16) < 65536
    } else {
      (allCatch opt s.toShort).isDefined
    }
  }
  def isDword(s: String): Boolean = {
    if (s.contains("0x")) {
      (allCatch opt Long.parseLong(s.drop(2), 16)).isDefined
    } else {
      (allCatch opt s.toLong).isDefined
    }
  }

  def asmMacro(c: Context)(args: c.Expr[Any]*): c.Expr[List[InstructionResult]] = {
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

    //c.abort(c.enclosingPosition, interleved.toString)

    val withSeparators: Seq[Any] = interleved.flatMap { x =>
      x match {
        case x: String => {
          var lines = x.split("\\r?\\n").toList.map{x => 
              val commentLoc = x.indexOf("//")
              if (commentLoc == -1) { // no comment found
                x
              } else {
                x.substring(0, commentLoc)
              }
            }
          
          if (lines.last.trim.isEmpty && lines.size > 2) {
            lines.head +: lines.tail.dropRight(1).flatMap { x => List(0, x) }
          } else {
            lines.head +: lines.tail.flatMap { x => List(0, x) }
          }
          
        }
        case x: c.Expr[_] => List(x)
        case _            => Nil
      }
    }

    //c.abort(c.enclosingPosition, "SEP: " + withSeparators.toString)

    def splitBySeparator(l: Seq[Any], sep: Any): Seq[Seq[Any]] = {
      import collection.mutable.ListBuffer
      val b = ListBuffer(ListBuffer[Any]())
      l foreach { e =>
        if (e == sep) {
          if (!b.last.isEmpty) {
            b += ListBuffer[Any]()
          }
        } else {
          b.last += e
        }
      }
      b.map(_.toSeq).filterNot{x => x.size == 1 && x.head.isInstanceOf[String] && x.head.asInstanceOf[String].trim.isEmpty}
    }

    //c.abort(c.enclosingPosition, splitBySeparator(withSeparators, 0).toString)

    val translated = splitBySeparator(withSeparators, 0).map { line =>

      val newArgs = line.collect { case x: c.Expr[_] => x }
      val inst = line.collect { case x: String => x }.toList

      processLine(c)(inst, newArgs: _*)
    }

    c.Expr(c.parse("List(" + translated.reduce { _ + ", " + _ } + ")"))
  }

  def processLine(c: Context)(inst: List[String], args: c.Expr[Any]*): String = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }
    //if (newArgs.size == 0)
    // c.abort(c.enclosingPosition, "inst:" + inst + " new args: " + newArgs)

    val asmInstruction = (if (inst.isEmpty) args(0).tree.symbol.name.decodedName.toString else inst.head).replaceAll("\\s+", " ").trim

    val params = Seq[Tree]((args map (_.tree)): _*)

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
            s"""LabelRef(\"$varName\", implicitly[JNZ#OneOp[Constant[_8]]])"""
          case "JZ" =>
            s"""LabelRef(\"$varName\", implicitly[JZ#OneOp[Constant[_8]]])"""
          case "JE" =>
            s"""LabelRef(\"$varName\", implicitly[JE#OneOp[Constant[_8]]])"""
          case "JMP" =>
            s"""LabelRef(\"$varName\", implicitly[JMP#OneOp[Constant[_8]]])"""
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

}


