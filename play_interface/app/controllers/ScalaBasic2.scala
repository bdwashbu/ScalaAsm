package com.play_interface

import java.io._
//import com.scalaAsm.ExeGenerator
//import com.scalaAsm.OptionalHeader
import scala.util.parsing.combinator.JavaTokenParsers
import com.scalaAsm.asm.Tokens.Variable
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.x86.Operands._
import scala.collection.mutable.HashSet
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.Tokens.CodeToken
import com.scalaAsm.asm.{ x86_32, x86_64 }
import com.scalaAsm.x86.r32

trait Inst
case class ADD_OP(x: Inst, y: Inst) extends Inst
case class MINUS_OP(x: Inst, y: Inst) extends Inst
case class TIMES_OP(x: Inst, y: Inst) extends Inst
case class DIVIDE_OP(x: Inst, y: Inst) extends Inst

case class IMM(x: Int) extends Inst
case class RESULT(code: Seq[AsmProgram[x86_32]#CodeSection#Code], reg: Operand[r32], availableRegs: Set[Operand[r32]]) extends Inst

class Arith extends JavaTokenParsers {

  def expr: Parser[Inst] = term ~ rep(plus | minus) ^^ { case a ~ b => (a /: b)((acc, f) => f(acc)) }
  def plus: Parser[Inst => Inst] = "+" ~ term ^^ { case "+" ~ b => ADD_OP(_, b) }
  def minus: Parser[Inst => Inst] = "-" ~ term ^^ { case "-" ~ b => MINUS_OP(_, b) }
  def term: Parser[Inst] = factor ~ rep(times | divide) ^^ { case a ~ b => (a /: b)((acc, f) => f(acc)) }
  def times: Parser[Inst => Inst] = "*" ~ factor ^^ { case "*" ~ b => TIMES_OP(_, b) }
  def divide: Parser[Inst => Inst] = "/" ~ factor ^^ { case "/" ~ b => DIVIDE_OP(_, b) }
  def factor: Parser[Inst] = fpn | "(" ~> expr <~ ")"
  def fpn: Parser[Inst] = floatingPointNumber ^^ { case x => IMM(x.toInt) }

}

object x86Parser {
  def parse(expression: String) = {

    new Arith with AsmProgram[x86_32] {

      import com.scalaAsm.x86.Instructions.Standard._

      sections += new DataSection {
        builder += Variable("helloWorld", expression + " = %d\r\n\u0000")
        builder += Variable("pressAnyKey", "Press any key to continue ...\u0000")
      }

      sections += new CodeSection {

        val STD_OUTPUT_HANDLE = byte(-11)
        val STD_INPUT_HANDLE = byte(-10)

        def transform(inst: Inst, resultReg: Operand[r32], availableRegs: Set[Operand[r32]]): RESULT = {

          def recurse(inst: Inst): RESULT = {
          
            inst match {
              // initial conditions
              case ADD_OP(IMM(x), IMM(y)) => {
                val code = Code(mov(resultReg, dword(x.toInt)), add(resultReg, byte(y.toInt)))
                RESULT(Seq(code), resultReg, availableRegs - resultReg)
              }
              case MINUS_OP(IMM(x), IMM(y)) => {
                val code = Code(mov(resultReg, dword(x.toInt)), sub(resultReg, byte(y.toInt)))
                RESULT(Seq(code), resultReg, availableRegs - resultReg)
              }
              case TIMES_OP(IMM(x), IMM(y)) => {
                val code = Code(push(eax), mov(eax, dword(x.toInt)), mov(resultReg, dword(y.toInt)), mul(resultReg), mov(resultReg, eax), pop(eax))
                RESULT(Seq(code), resultReg, availableRegs - resultReg)
              }
              // answer propagation
              case ADD_OP(IMM(x), RESULT(code2, y, aregs)) => {
                val head = aregs.head
                val rem = aregs.tail
                val code = Code(mov(head, dword(x.toInt)), add(head, y))
                RESULT(code2 ++: Seq(code), head, rem + y)
              }
              case ADD_OP(RESULT(code, x, aregs), IMM(z)) => {
                recurse(ADD_OP(IMM(z), RESULT(code, x, aregs)))
              }
              case MINUS_OP(IMM(x), RESULT(code2, y, aregs)) => {
                val head = aregs.head
                val rem = aregs.tail
                val code = Code(mov(head, dword(x.toInt)), add(head, y))
                //val code = Code(sub(resultReg, byte(x.toInt)))
                RESULT(code2 ++: Seq(code), resultReg, aregs + y)
              }
              case MINUS_OP(RESULT(code2, x, aregs), IMM(z)) => {
                val code = if (resultReg != x) {
                  Code(mov(resultReg, x), sub(resultReg, byte(z.toInt)))
                } else {
                  Code(sub(resultReg, byte(z.toInt)))
                }
                RESULT(code2 ++: Seq(code), resultReg, aregs + x)
              }
              case TIMES_OP(IMM(x), RESULT(code2, y, aregs)) => {
                val code = Code(push(eax), mov(eax, dword(x.toInt)), mov(resultReg, y), mul(resultReg), mov(resultReg, eax), pop(eax))
                RESULT(code2 ++: Seq(code), resultReg, aregs)
              }
              case TIMES_OP(RESULT(code, y, aregs), IMM(x)) => {
                recurse(TIMES_OP(IMM(x), RESULT(code, y, aregs)))
              }
              case ADD_OP(RESULT(code1, x, aregs), RESULT(code2, z, aregs2)) => {
                val combo = aregs ++ aregs2
                val head = combo.head
                val rem = combo.tail
                val code = Code(mov(resultReg, x), add(resultReg, z))
                RESULT(code2 ++: Seq(code), resultReg, rem + z)
              }
              case MINUS_OP(RESULT(code1, x, aregs), RESULT(code2, z, aregs2)) => {
                val combo = aregs ++ aregs2
                val head = combo.head
                val rem = combo.tail
                val code = Code(mov(resultReg, x), sub(resultReg, z))
                RESULT(code2 ++: Seq(code), resultReg, aregs ++ aregs2 + z)
              }
              case TIMES_OP(RESULT(code1, x, aregs), RESULT(code2, y, aregs2)) => {
                val combo = aregs ++ aregs2
                val head = combo.head
                val rem = combo.tail
                val code = Code(push(eax), mov(eax, x), mov(resultReg, y), mul(resultReg), mov(resultReg, eax), pop(eax))
                RESULT(code2 ++: Seq(code), resultReg, aregs ++ aregs2)
              }
              case ADD_OP(x, IMM(y))   => recurse(ADD_OP(recurse(x), IMM(y)))
              case MINUS_OP(x, IMM(y)) => recurse(MINUS_OP(recurse(x), IMM(y)))
              case TIMES_OP(x, IMM(y)) => recurse(TIMES_OP(recurse(x), IMM(y)))
              case ADD_OP(IMM(x), y)   => recurse(ADD_OP(IMM(x), recurse(y)))
              case MINUS_OP(IMM(x), y) => recurse(MINUS_OP(IMM(x), recurse(y)))
              case TIMES_OP(IMM(x), y) => recurse(TIMES_OP(IMM(x), recurse(y)))
              case ADD_OP(x, y)        => {
                recurse(ADD_OP(recurse(x), recurse(y)))
              }
              case MINUS_OP(x, y)      => {
                recurse(MINUS_OP(recurse(x), recurse(y)))
              }
              case TIMES_OP(x, y)      => {
                recurse(TIMES_OP(recurse(x), recurse(y)))
              }
              case _                   => if (inst != null) println(inst); null
            }
          }
          recurse(inst)
        }

        val result = parseAll(expr, expression).get
        val transformed = transform(result, ebp, Set(edi, eax, ebx, ecx, edx))

        builder += Code(transformed.code.map(_.code).reduce{ _ ++ _}: _*)
        
        builder += Code(
          push(transformed.reg),
          push("helloWorld"),
          call("printf"),
          pop(ebx),
          pop(ebx),
          push("pressAnyKey"),
          call("printf"),
          pop(ebx),
          push(STD_INPUT_HANDLE),
          call("GetStdHandle"),
          push(eax),
          call("FlushConsoleInputBuffer"),
          call("_getch"),
          retn(()))
      }
    }
  }
}

//object ScalaBasic2 extends Arith {
//
//  def parseExpr(expr: String) = {
//    
//  }
//  
//  def main(args: Array[String]): Unit = {
//    try {
//
//        //val input = "2*2*2*2" 
//        val input = "3*(1 + 2)"
//        
//        
//        
//         val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
//         val assembled = x86Parser.parse(input).assemble
//         val exe = ExeGenerator.compile(assembled, 0x2000)
//         outputStream.write(exe.get)
//
//         println("done generating")
//
//         outputStream.close
//        
//          println(parseAll(expr, input).get) // prints 33.0 
//          //println(transformed)
//    } catch {
//      case e: Exception => e.printStackTrace()
//    }
//  }
//
//}