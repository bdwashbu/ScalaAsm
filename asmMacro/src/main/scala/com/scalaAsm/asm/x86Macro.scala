package com.scalaAsm.asm

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.currentMirror

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

import scala.reflect.runtime.universe._
import scala.util.control.Exception.allCatch
import java.lang.Long;
import java.lang.Byte;
import scala.reflect.macros.{ TypecheckException }

object x86Macro {

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

  //   def parseParam(c: Context, param: String): c.Tree = {
  //     param match {
  //        case Dword(dword)       => c.parse(s"byte($dword))")
  //        case "Long" | "Int" => c.Expr(q"$mnemonic($param, dword($param2))")
  //        case _              => c.Expr(q"$mnemonic($param, $param2)")
  //      }
  //   }

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

    (valdefs(name), typeName) match {
      case (Dword(dword), _)    => s"dword($dword))"
      case (_, "Int")           => s"dword($name)"
      case (Byte(byteVal), _)   => s"byte($byteVal)"
      case (Memory(byteVal), _) => s"$byteVal"
      case _                    => s"$param"
    }
  }

  def parseInterpolated(c: Context, asmInstructions: List[String])(args: Seq[c.Expr[Any]]): c.Expr[InstructionResult] = {
    import c.universe._

    val fullInst = asmInstructions.reduce(_ + "" + _)
    val asmInstruction = asmInstructions.head

    //c.abort(c.enclosingPosition, asmInstructions.reduce(_ + " | " + _) + ":" + params.toString)

    val mnemonic = asmInstruction.split(' ').head.toUpperCase
    //throw new Exception(params.head.toString())
    val inst = if (!fullInst.contains(',')) {
      val param = parseParam(c)(args(0).tree)
      s"$mnemonic($param)"
    } else if (asmInstructions.size == 2 && args.size == 1 && asmInstructions.last.size == 0) { // interpolated var at end of string
      val param = asmInstruction.split(' ').tail.mkString.split(',').head
      val param2 = parseParam(c)(args(0).tree)
      s"$mnemonic($param, $param2)"
    } else {
      val param = asmInstructions.last.split(' ').tail.mkString.split(',').head
      val x = parseParam(c)(args(0).tree)
      s"$mnemonic($x, $param)"
    }

    c.Expr(c.parse(s"$inst"))
  }

  object Byte {
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
      if (operand.split(" ").head == "byte" && isByte(operand.split(" ").last)) {
        //throw new Exception("WTF")
        Some(convertByte(operand.split(" ").last))
      } else if (!operand.contains(" ") && isByte(operand)) {
        //throw new Exception("WTF2")
        Some(convertByte(operand))
      } else {
        None
      }
    }
  }

  object Dword {
    def convertDword(operand: String): String = {
      if (operand.startsWith("0x")) {
        (Long.parseLong(operand.drop(2), 16)).toInt.toString
      } else {
        operand
      }
    }

    def unapply(operand: String): Option[String] = {
      if (operand.split(" ").head == "dword" && isDword(operand.split(" ").last)) {
        Some(convertDword(operand.split(" ").last))
      } else if (isDword(operand)) {
        Some(convertDword(operand))
      } else {
        None
      }
    }
  }

  object Memory {
    def unapply(operand: String): Option[String] = {

      if (operand.startsWith("[") && operand.endsWith("]")) {
        val trimmed = operand.drop(1).dropRight(1)
        if (operand.contains("+")) { // base index addressing
          val tokens = trimmed.split(" ")
          val reg = TermName(tokens(0))

          tokens(2) match {
            case Byte(byteVal) => Some(s"$reg + byte($byteVal.toByte)")
            case Dword(dword)  => Some(s"$reg + dword($dword.toInt)")
          }

        } else if (operand.contains("-")) { // base index addressing
          val tokens = trimmed.split(" ")
          val reg = TermName(tokens(0))

          tokens(2) match {
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

  def x86(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

    object TwoOperands {
      def unapply(line: List[String]): Option[(String, String, String)] = {
        val mnemonic = line.head.split(' ').head.toUpperCase()
        if (line.head.contains(' ') && line.head.contains(',')) {
          val params = line.head.split(' ').tail.reduce(_ + " " + _).split(',').map { param =>
            if (param.contains("(")) {
              param.trim.split("(").last.split(")").head
            } else {
              param.trim
            }
          }
          Some(mnemonic, params(0), params(1))
        } else {
          None
        }
      }
    }

    object OneOperand {
      def unapply(line: List[String]): Option[(String, String)] = {
        val mnemonic = line.head.split(' ').head.toUpperCase()
        if (line.head.contains(' ') && !line.head.contains(',')) {
          Some(mnemonic, line.head.split(' ').last)
        } else {
          None
        }
      }
    }

    object NoOperand {
      def unapply(line: List[String]): Option[String] = {
        if (!line.head.contains(' ')) {
          Some(line.head.split(' ').head.toUpperCase())
        } else {
          None
        }
      }
    }

    object Register {
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

    val asmInstructions = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x
      }
      case _ => Nil
    })

    def checkTypeCheck(reg: String, line: String, mnemonic: String) = {
      val expr = if (line.contains(" ")) {
        s"$mnemonic($reg, byte(${line.split(" ").last}))"
      } else {
        s"$mnemonic($reg, byte($line))"
      }

      val testType = scala.util.Try(c.typecheck(c.parse("{ " + expr + " }")))

      testType.isSuccess
    }

    if (!args.isEmpty) { // contains an interpolated value
      parseInterpolated(c, asmInstructions)(args)
    } else {
      val inst = asmInstructions match {
        case NoOperand(mnemonic) =>
          s"$mnemonic(())"
        case OneOperand(mnemonic, param) =>
          param match {
            case Memory(mem)   => s"$mnemonic($mem)"
            case Register(reg) => s"$mnemonic($reg)"
            case Dword(dword)  => s"$mnemonic(dword($dword.toInt))"
          }
        case TwoOperands(mnemonic, operand1, operand2) =>
          val mnem = mnemonic
          (operand1, operand2) match {
            case (Register(reg1), Memory(mem))    => s"$mnemonic($reg1, $mem)"
            case (Register(reg1), Register(reg2)) => s"$mnemonic($reg1, $reg2)"
            case (Register(reg), Byte(byteVal)) if checkTypeCheck(operand1, operand2, asmInstructions.head.split(' ').head.toUpperCase()) => {

              s"$mnemonic($reg, byte($byteVal.toByte))"
            }
            case (Register(reg), Dword(dword)) => s"$mnemonic($reg, dword($dword.toInt))"
          }
      }
      c.Expr(c.parse(s"$inst"))
    }
  }

}