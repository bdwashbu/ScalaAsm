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

object AsmCompiler {

  val regList = Seq("ebx", "ebp", "eax", "ecx", "edx", "esp", "edi", "cl", "rsp", "rax", "spl")

  def isByte(s: String): Boolean = {
    if (s.contains("0x")) {
      Long.parseLong(s.drop(2), 16) < 128
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

    val asmInstruction = (c.prefix.tree match {
      case Apply(_, List(Apply(_, xs))) => xs map {
        case Literal(Constant(x: String)) => x
      }
      case _ => Nil
    }).head

    val params = Seq[Tree]((args map (_.tree)): _*)

    if (!params.isEmpty) {
      x86Macro(c)(args: _*)
    } else if (asmInstruction.endsWith(":")) { // label
      val labelName = Constant(asmInstruction.reverse.tail.reverse)
      c.Expr(q"Label($labelName)")
    } else if (asmInstruction.contains(' ') && !asmInstruction.contains(',')) {
      val mnemonic = asmInstruction.split(' ').head.toUpperCase()
      val param = asmInstruction.split(' ').last
      if (regList contains param) {
        x86Macro(c)(args: _*)
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
          case _        => x86Macro(c)(args: _*)
        }
      } else {
        x86Macro(c)(args: _*)
      }
    } else {
      x86Macro(c)(args: _*)
    }
  }

  def parseInterpolated(c: Context, asmInstructions: List[String])(args: Seq[c.Expr[Any]]): c.Expr[InstructionResult] = {
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

    val fullInst = asmInstructions.reduce(_ + "" + _)
    val asmInstruction = asmInstructions.head
    val params = Seq[Tree]((args map (_.tree)): _*)

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
        case "Byte"         => c.Expr(q"$mnemonic($param, byte($param2))")
        case "Long" | "Int" => c.Expr(q"$mnemonic($param, dword($param2))")
        case _              => c.Expr(q"$mnemonic($param, $param2)")
      }
    } else {
      val param = TermName(asmInstructions.last.split(' ').tail.mkString.split(',').head)
      val x = q"${args(0)}"
      c.Expr(q"$mnemonic($x, $param)")
    }
  }

  object TwoOperands {
    def unapply(line: String): Option[(String, String)] = {
      if (line.contains(' ') && line.contains(',')) {
        val params = line.split(' ').tail.reduce(_ + " " + _).split(',').map { param =>
          if (param.contains("(")) {
            param.trim.split("(").last.split(")").head
          } else {
            param.trim
          }
        }
        Some(params(0), params(1))
      } else {
        None
      }
    }
  }

 

  def x86Macro(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

     object OneOperand {
        def unapply(line: String): Option[String] = {
          if (line.contains(' ') && !line.contains(',')) {
            Some(line.split(' ').last)
          } else {
            None
          }
        }
      }
    
      object NoOperand {
        def unapply(line: String): Boolean = !line.contains(' ')
      }
      
      object Register {
        def unapply(operand: String): Option[TermName] = {
          if (regList contains operand) {
            Some(TermName(operand))
          } else {
            None
          }
        }
      }
      
      object Dword {
        def convertDword(operand: String): String = {
          if (operand.startsWith("0x")) {
              (Long.parseLong(operand.drop(2), 16).toInt).toString
            } else {
              operand
            }
        }
        
        def unapply(operand: String): Option[Constant] = {
         if (operand.split(" ").head == "dword" && isDword(convertDword(operand.split(" ").last))) {
            //throw new Exception("WTF")
            Some(Constant(convertDword(operand.split(" ").last)))
          } else if (isDword(convertDword(operand))) {
            //throw new Exception("WTF2")
            Some(Constant(convertDword(operand)))
          } else {
            None
          }
        }
      }
      
      object Byte {
        def convertByte(operand: String): String = {
          if (operand.startsWith("0x")) {
              (Long.parseLong(operand.drop(2), 16).toByte).toString
            } else {
              operand
            }
        }
        
        def unapply(operand: String): Option[Constant] = {
          if (operand.split(" ").head == "byte" && isByte(convertByte(operand.split(" ").last))) {
            //throw new Exception("WTF")
            Some(Constant(convertByte(operand.split(" ").last)))
          } else if (isByte(convertByte(operand))) {
            //throw new Exception("WTF2")
            Some(Constant(convertByte(operand)))
          } else {
            None
          }
        }
      }
    
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

    if (!args.isEmpty) { // contains an interpolated value
      parseInterpolated(c, asmInstructions)(args)
    } else {
      val asmInstruction = asmInstructions.head
      val mnemonic = TermName(asmInstruction.split(' ').head.toUpperCase())

      asmInstruction match {
        case NoOperand() =>
          c.Expr(q"$mnemonic(())")
        case OneOperand(param) =>
          param match {
            case Register(reg) => c.Expr(q"$mnemonic($reg)")
            case Dword(dword) => c.Expr(q"$mnemonic(dword($dword.toInt))")
          }
        case TwoOperands(operand1, operand2) =>
          (operand1, operand2) match {
            case (Register(reg1), Register(reg2)) => c.Expr(q"$mnemonic($reg1, $reg2)")
            case (Register(reg), Dword(dword)) =>  c.Expr(q"$mnemonic($reg, dword($dword.toInt))")
            case (Register(reg), Byte(byteVal)) => c.Expr(q"$mnemonic($reg, byte($byteVal.toByte))")
            
          }

          
//          } else {
//            c.Expr(Apply(Select(This(TypeName("$anon")), mnemonic), List(Literal(Constant(())))))
//          }
      }

      //Expr(Apply(Select(Select(Ident(TermName("$anon")), TermName("mov")), TermName("apply")), List(Select(Ident(TermName("$anon")), TermName("ebp")), Apply(Select(Ident(TermName("$anon")), TermName("esp")))))))

    }
  }

}