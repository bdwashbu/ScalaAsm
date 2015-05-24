package com.scalaAsm.asm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.currentMirror

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._
import scala.util.Try

import scala.reflect.runtime.universe._
import scala.util.control.Exception.allCatch
import java.lang.Long;
import java.lang.Byte;
import scala.reflect.macros.{ TypecheckException }

object x86Macro {

  

  def parseParam(c: Context)(param: c.Tree): String = {
    import c.universe._

    val name = param.symbol.name.decodedName.toString()
    val typeName = param.tpe.typeSymbol.name.decodedName.toString

    val valdefs = c.enclosingImpl.collect {
      case ValDef(_, name, _, rhs) => {
        if (rhs.toString.contains("\"")) {
          name.toString -> rhs.toString.drop(1).dropRight(1)
        } else {
          name.toString -> rhs.toString
        }
      }
    }.toMap.withDefaultValue(name)

    //c.abort(c.enclosingPosition, valdefs(name))
    val valDef = valdefs(name)

    (valDef, typeName) match {
      case (_, "Int")           => s"dword($valDef)"
      case (_, "String")        => valDef
      case _                    => valDef
    }
  }

  def parseInterpolated(c: Context, asmInstructions: List[String])(args: Seq[c.Expr[Any]]): String = {
    import c.universe._

    def interleave(xs: Seq[String], ys: Seq[String], n: Int): Seq[String] = {
      val iter = xs grouped n
      val coll = iter zip ys.iterator flatMap { case (xs, y) => if (xs.size == n) xs :+ y else xs }
      (coll ++ iter.flatten).toIndexedSeq
    }
    
    val interleved = interleave(asmInstructions, args.map{x => parseParam(c)(x.tree)}, 1)
    val inst = interleved.reduce{_ + "" + _}
 
    inst
  }

  object Byte {
    def isByte(s: String): Boolean = {
      val result = Try(if (s.contains("0x")) {
        Long.parseLong(s.drop(2), 16) < 256
      } else {
        Long.parseLong(s, 10) < 256
      })
      result.getOrElse(false)
    }
     
    def convertByte(operand: String): String = {
      if (operand.startsWith("0x")) {
        val result = Long.parseLong(operand.drop(2), 16)
        //              if (result > 255) {
        //                c.abort(c.enclosingPosition, s"Error: Value '$result' is too large for a byte")
        //              }

        Long.parseLong(operand.drop(2), 16).toByte.toString
      } else {
        operand
      }
    }

    def unapply(operand: String): Option[String] = {
      if (operand.split(" ").head != "dword") {
        if (operand.split(" ").head == "byte" && isByte(operand.split(" ").last)) {
          //throw new Exception("WTF")
          Some(convertByte(operand.split(" ").last))
        } else if (!operand.contains(" ") && isByte(operand)) {
          //throw new Exception("WTF2")
          Some(convertByte(operand))
        } else {
          None
        }
      } else {
        None
      }
    }
  }
  
  object Word {
    def isWord(s: String): Boolean = {
      val result = Try(if (s.contains("0x")) {
        Long.parseLong(s.drop(2), 16) < 65536
      } else {
        Long.parseLong(s, 10) < 65536
      })
      result.getOrElse(false)
    }
     
    def convertWord(operand: String): String = {
      if (operand.startsWith("0x")) {
        val result = Long.parseLong(operand.drop(2), 16)
        //              if (result > 255) {
        //                c.abort(c.enclosingPosition, s"Error: Value '$result' is too large for a byte")
        //              }

        Long.parseLong(operand.drop(2), 16).toShort.toString
      } else {
        operand
      }
    }

    def unapply(operand: String): Option[String] = {
      if (operand.split(" ").head != "byte" && operand.split(" ").head != "dword") {
        if (operand.split(" ").head == "word" && isWord(operand.split(" ").last)) {
          //throw new Exception("WTF")
          Some(convertWord(operand.split(" ").last))
        } else if (!operand.contains(" ") && isWord(operand)) {
          //throw new Exception("WTF2")
          Some(convertWord(operand))
        } else {
          None
        }
      } else {
        None
      }
    }
  }

  object Dword {
    def isDword(s: String): Boolean = {
      if (s.contains("0x")) {
        (allCatch opt Long.parseLong(s.drop(2), 16)).isDefined
      } else {
        (allCatch opt s.toLong).isDefined
      }
    }
    
    def convertDword(operand: String): String = {
      if (operand.startsWith("0x")) {
        (Long.parseLong(operand.drop(2), 16)).toInt.toString
      } else {
        operand
      }
    }

    def unapply(operand: String): Option[String] = {
      if (operand.split(" ").head != "byte") {
        if (operand.split(" ").head == "dword" && isDword(operand.split(" ").last)) {
          Some(convertDword(operand.split(" ").last))
        } else if (isDword(operand)) {
          Some(convertDword(operand))
        } else {
          None
        }
      } else {
        None
      }
    }
  }

  object Memory {
    def unapply(operand: String): Option[String] = {

      if (operand.startsWith("[") && operand.endsWith("]")) {
        val trimmed = operand.drop(1).dropRight(1)
        if (operand.contains("+") && operand.count(_ == '+') == 1) { // base index addressing
          val tokens = trimmed.split('+').map(_.trim)
          val reg = TermName(tokens(0))

          tokens(1) match {
            case Byte(byteVal) => Some(s"$reg + byte($byteVal.toByte)")
            case Dword(dword)  => Some(s"$reg + dword($dword.toInt)")
          }

        } else if (operand.contains("-") && operand.count(_ == '-') == 1) { // base index addressing
          val tokens = trimmed.split('-').map(_.trim)
          val reg = TermName(tokens(0))

          tokens(1) match {
            case Byte(byteVal) => Some(s"$reg - byte($byteVal.toByte)")
            case Dword(dword)  => Some(s"$reg - dword($dword.toInt)")
          }

        } else { // register indirect addressing
          val reg = operand.drop(1).dropRight(1)
          Some(s"$reg.Indirect()")
        }
      } else {
        None
      }
    }
  }

  def x86Mac(c: Context)(args: c.Expr[Any]*): String = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }
    
    val asmInstructions = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x.replaceAll("\\s+", " ").trim
      }
      case _ => Nil
    })
    
    x86(c, asmInstructions)(args: _*)
  }
  
  def x86(c: Context, line: List[String])(args: c.Expr[Any]*): String = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

    object TwoOperands {
      def unapply(line: String): Option[(String, String, String)] = {
        val mnemonic = line.split(' ').head.toUpperCase()
        val index = line.indexOf(" ")
        val paramString = line.splitAt(index+1)._2
        if (paramString.contains(',')) {
          
          val params = paramString.split(',').map { param =>
            //if (param.contains("(")) {
            //  param.trim.split("(").last.split(")").head
            //} else {
              param.trim
            //}
          }
          Some(mnemonic, params(0), params(1))
        } else {
          None
        }
      }
    }

    object OneOperand {
      def unapply(line: String): Option[(String, String)] = {
        val mnemonic = line.split(' ').head.toUpperCase()
        if (line.contains(' ') && !line.contains(',')) {
          val index = line.indexOf(" ")
          Some(mnemonic, line.splitAt(index+1)._2)
        } else {
          None
        }
      }
    }

    object NoOperand {
      def unapply(line: String): Option[String] = {
        if (!line.contains(' ')) {
          Some(line.split(' ').head.toUpperCase())
        } else {
          None
        }
      }
    }

    object Register {
      val regList = Seq("ebx", "ebp", "eax", "ecx", "edx", "esp", "edi", "cl", "rsp", "rax", "spl", "rdx")
      
      def unapply(operand: String): Option[String] = {
        if (regList contains operand) {
          Some(operand)
        } else {
          None
        }
      }
    }

    //val toolBox = currentMirror.mkToolBox()
    //val importer = c.universe.mkImporter(ru)

    def checkForImplicit(mnemonic: String, arg0: String) = {
      val expr = s"$mnemonic($arg0)"

      val testType = scala.util.Try(c.typecheck(c.parse("{ " + expr + " }")))

      testType.isSuccess
    }

    def checkForImplicit2(mnemonic: String, arg0: String, arg1: String) = {
      val expr = s"$mnemonic($arg0, $arg1)"

      val testType = scala.util.Try(c.typecheck(c.parse("{ " + expr + " }")))

      testType.isSuccess
    }

    //val blah = parseInterpolated(c, line)(args)
    
    //val result = if (!args.isEmpty) { // contains an interpolated value
    //  parseInterpolated(c, line)(args)
    //} else {
      //c.abort(c.enclosingPosition, line.head)
      val trimmed = parseInterpolated(c, line)(args).replaceAll("\\s+", " ").trim
      //c.abort(c.enclosingPosition, "blar: " + trimmed)
      val result = trimmed match {
        case NoOperand(mnemonic) =>
          s"$mnemonic(())"
        case OneOperand(mnemonic, param) =>
          param match {
            case Memory(mem)   => s"$mnemonic($mem)"
            case Register(reg) => s"$mnemonic($reg)"
            case Dword(dword) if checkForImplicit(mnemonic, s"dword($dword.toInt)") => s"$mnemonic(dword($dword.toInt))"
            case Byte(byteVal) if checkForImplicit(mnemonic, s"byte($byteVal.toByte)") => s"$mnemonic(byte($byteVal.toByte))"
            case Word(word)  => s"$mnemonic(word($word.toShort))"
            
            case x: String     => s"$mnemonic($x)"
          }
        case TwoOperands(mnemonic, operand1, operand2) =>
          val mnem = mnemonic
          //c.abort(c.enclosingPosition, "dork: " + trimmed)
          (operand1, operand2) match {
            case (Register(reg), Memory(mem))    => s"$mnemonic($reg, $mem)"
            case (Memory(mem), Register(reg))    => s"$mnemonic($mem, $reg)"
            case (Register(reg1), Register(reg2)) => s"$mnemonic($reg1, $reg2)"
            case (Register(reg), Byte(byteVal)) if checkForImplicit2(mnemonic, reg, s"byte($byteVal.toByte)") => {
              s"$mnemonic($reg, byte($byteVal.toByte))"
            }
            case (Register(reg), Dword(dword)) => s"$mnemonic($reg, dword($dword.toInt))"
            case (Register(reg), x: String) => s"$mnemonic($reg, $x)"
            case (x: String, Register(reg)) => s"$mnemonic($x, $reg)"
          }
      }
    //c.abort(c.enclosingPosition, "blax: " + result)
    
    val sanity = scala.util.Try(c.typecheck(c.parse("{ " + result + " }")))
    if (sanity.isFailure) {
      //c.abort(c.enclosingPosition, "Error: does not compile: " + result)
    }
    result
  }

}