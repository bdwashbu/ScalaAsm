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
      if (regList.contains(param) || (param.contains("[") && param.contains("]"))) {
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

    val fullInst = asmInstructions.reduce(_ + "" + _)
    val asmInstruction = asmInstructions.head
    val interpolatedArgs = Seq[Tree]((args map (_.tree)): _*)
    
    //c.abort(c.enclosingPosition, asmInstructions.reduce(_ + " | " + _) + ":" + params.toString)

    val mnemonic = TermName(asmInstruction.split(' ').head.toUpperCase)
    //throw new Exception(params.head.toString())
    if (interpolatedArgs.head.toString contains "toString") {
      val param = TermName(asmInstruction.split(' ').tail.mkString.split(',').head)
      // throw new Exception("kookoo")
      c.Expr(q"$mnemonic($param, dword(input))")
    } else if (!fullInst.contains(',')) {
      //c.abort(c.enclosingPosition, asmInstructions(0).toString)
      //throw new Exception("fuckcck")
      val x = q"${interpolatedArgs(0)}"
      if (isByte(x.toString)) {
        c.Expr(q"$mnemonic(byte($x.toByte))")
      } else {
        c.Expr(q"$mnemonic($x)")
      }
    } else if (asmInstructions.size == 2 && interpolatedArgs.size == 1 && asmInstructions.last.size == 0) { // interpolated var at end of string
      val param = TermName(asmInstruction.split(' ').tail.mkString.split(',').head)
      val param2 = TermName(interpolatedArgs(0).toString.split('.').last)

      interpolatedArgs(0).tpe.typeSymbol.name.toString match {
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

  

 

  def x86Macro(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
    import c.universe._
    import scala.reflect.runtime.{ currentMirror => cm }
    import scala.reflect.runtime.{ universe => ru }

     object TwoOperands {
      def unapply(line: List[String]): Option[(TermName, String, String)] = {
        val mnemonic = TermName(line.head.split(' ').head.toUpperCase())
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
        def unapply(line: List[String]): Option[(TermName, String)] = {
          val mnemonic = TermName(line.head.split(' ').head.toUpperCase())
          if (line.head.contains(' ') && !line.head.contains(',')) {
            Some(mnemonic, line.head.split(' ').last)
          } else {
            None
          }
        }
      }
    
      object NoOperand {
        def unapply(line: List[String]): Option[TermName] = {
          if (!line.head.contains(' ')) {
            Some(TermName(line.head.split(' ').head.toUpperCase()))
          } else {
            None
          }
        }
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
      
      object Memory {
        def unapply(operand: String): Option[c.Expr[InstructionResult]] = {

         if (operand.startsWith("[") && operand.endsWith("]")) {
           
           val reg = TermName(operand.drop(1).dropRight(1))
            Some(c.Expr(q"$reg.Indirect()"))
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
            Some(Constant(convertDword(operand.split(" ").last)))
          } else if (isDword(convertDword(operand))) {
            Some(Constant(convertDword(operand)))
          } else {
            None
          }
        }
      }
      
      object Byte {
        def convertByte(operand: String): String = {
          if (operand.startsWith("0x")) {
              val result = Long.parseLong(operand.drop(2), 16)
              if (result > 255) {
                c.abort(c.enclosingPosition, s"Error: Value '$result' is too large for a byte")
              }
              
              Long.parseLong(operand.drop(2), 16).toByte.toString
            } else {
              operand
            }
        }
        
        def unapply(operand: String): Option[Constant] = {
          if (operand.split(" ").head == "byte" && isByte(operand.split(" ").last)) {
            //throw new Exception("WTF")
            Some(Constant(convertByte(operand.split(" ").last)))
          } else if (isByte(operand)) {
            //throw new Exception("WTF2")
            Some(Constant(convertByte(operand)))
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

    if (!args.isEmpty) { // contains an interpolated value
      parseInterpolated(c, asmInstructions)(args)
    } else {
      asmInstructions match {
        case NoOperand(mnemonic) =>
          c.Expr(q"$mnemonic(())")
        case OneOperand(mnemonic, param) =>
          param match {
            case Memory(mem) => c.Expr(q"$mnemonic($mem)")
            case Register(reg) => c.Expr(q"$mnemonic($reg)")
            case Dword(dword) => c.Expr(q"$mnemonic(dword($dword.toInt))")
          }
        case TwoOperands(mnemonic, operand1, operand2) =>
          (operand1, operand2) match {
            case (Register(reg1), Memory(mem)) => c.Expr(q"$mnemonic($reg1, $mem)")
            case (Register(reg1), Register(reg2)) => c.Expr(q"$mnemonic($reg1, $reg2)")
            case (Register(reg), Byte(byteVal)) => c.Expr(q"$mnemonic($reg, byte($byteVal.toByte))")
            case (Register(reg), Dword(dword)) =>  c.Expr(q"$mnemonic($reg, dword($dword.toInt))")

          }

          
//          } else {
//            c.Expr(Apply(Select(This(TypeName("$anon")), mnemonic), List(Literal(Constant(())))))
//          }
      }

      //Expr(Apply(Select(Select(Ident(TermName("$anon")), TermName("mov")), TermName("apply")), List(Select(Ident(TermName("$anon")), TermName("ebp")), Apply(Select(Ident(TermName("$anon")), TermName("esp")))))))

    }
  }

}