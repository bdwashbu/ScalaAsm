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
import scala.reflect.macros.{TypecheckException}

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
   
   def x86(c: Context)(args: c.Expr[Any]*): c.Expr[InstructionResult] = {
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
           val trimmed = operand.drop(1).dropRight(1)
           if (operand.contains("+")) { // base index addressing
             val tokens = trimmed.split(" ")
             val reg = TermName(tokens(0))
             
             tokens(2) match {
               case Byte(byteVal) => Some(c.Expr(q"$reg + byte($byteVal.toByte)"))
               case Dword(dword) => Some(c.Expr(q"$reg + dword($dword.toInt)"))
             }

           } else if (operand.contains("-")) { // base index addressing
             val tokens = trimmed.split(" ")
             val reg = TermName(tokens(0))
             
             tokens(2) match {
               case Byte(byteVal) => Some(c.Expr(q"$reg - byte($byteVal.toByte)"))
               case Dword(dword) => Some(c.Expr(q"$reg - dword($dword.toInt)"))
             }

           } else { // register indirect addressing
             val reg = TermName(operand.drop(1).dropRight(1))
              Some(c.Expr(q"$reg.Indirect()"))
           }
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
        
        def unapply(operand: String): Option[Constant] = {
         if (operand.split(" ").head == "dword" && isDword(operand.split(" ").last)) {
            Some(Constant(convertDword(operand.split(" ").last)))
          } else if (isDword(operand)) {
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
          } else if (!operand.contains(" ") && isByte(operand)) {
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

    def checkTypeCheck(reg: String, line: String, mnemonic: String) = {
      val poo = if (line.contains(" ")) {
        s"$mnemonic($reg, byte(${line.split(" ").last}))"
      } else {
        s"$mnemonic($reg, byte($line))"
      }
        
      var returnVal = true;
      
        try c.typeCheck(c.parse("{ "+poo+" }")) catch { case e: TypecheckException =>
          val msg = e.getMessage
          returnVal = false
        }
        
      returnVal
    }
    
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
          val mnem = mnemonic
          (operand1, operand2) match {
            case (Register(reg1), Memory(mem)) => c.Expr(q"$mnemonic($reg1, $mem)")
            case (Register(reg1), Register(reg2)) => c.Expr(q"$mnemonic($reg1, $reg2)")
            case (Register(reg), Byte(byteVal)) if checkTypeCheck(operand1, operand2, asmInstructions.head.split(' ').head.toUpperCase()) => {
              
              c.Expr(q"$mnemonic($reg, byte($byteVal.toByte))")
            }
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