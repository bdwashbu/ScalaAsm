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
case class RESULT(code: Seq[AsmProgram[x86_32]#CodeSection#Code], reg: Operand[r32]) extends Inst

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

        def transform(inst: Inst, resultReg: Operand[r32]): RESULT = {

          inst match {
            case ADD_OP(IMM(x), IMM(y)) => {
              val code = Code(mov(resultReg, dword(x.toInt)), add(resultReg, byte(y.toInt)))
              RESULT(Seq(code), resultReg)
            }
            case MINUS_OP(IMM(x), IMM(y)) => {
              val code = Code(mov(resultReg, dword(x.toInt)), sub(resultReg, byte(y.toInt)))
              RESULT(Seq(code), resultReg)
            }
            case TIMES_OP(IMM(x), IMM(y)) => {
              val code = Code(mov(eax, dword(x.toInt)), mov(resultReg, dword(y.toInt)), mul(resultReg), mov(resultReg, eax))
              RESULT(Seq(code), resultReg)
            }
            case ADD_OP(IMM(x), RESULT(code2, y)) => {
              val code = if (resultReg != y) {
                Code(mov(resultReg, dword(x.toInt)), add(resultReg, y))
              } else {
                Code(add(resultReg, byte(x.toInt)))
              }
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case ADD_OP(RESULT(code, x), IMM(z)) => {
              transform(ADD_OP(IMM(z), RESULT(code, x)), resultReg)
            }
            case MINUS_OP(IMM(x), RESULT(code2, y)) => {
              val code = if (resultReg != y) {
                Code(mov(resultReg, dword(x.toInt)), sub(resultReg, y))
              } else {
                Code(sub(resultReg, byte(x.toInt)))
              }
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case MINUS_OP(RESULT(code2, x), IMM(z)) => {
              val code = if (resultReg != x) {
                Code(mov(resultReg, x), sub(resultReg, byte(z.toInt)))
              } else {
                Code(sub(resultReg, byte(z.toInt)))
              }
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case TIMES_OP(IMM(x), RESULT(code2, y)) => {
              val code = Code(mov(eax, dword(x.toInt)), mov(resultReg, y), mul(resultReg), mov(resultReg, eax))
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case TIMES_OP(RESULT(code, y), IMM(x)) => {
              transform(TIMES_OP(IMM(x), RESULT(code, y)), resultReg)
            }
            case ADD_OP(RESULT(code1, x), RESULT(code2, z)) => {
              val code = Code(mov(resultReg, x), add(resultReg, z))
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case MINUS_OP(RESULT(code1, x), RESULT(code2, z)) => {
              val code = Code(mov(resultReg, x), sub(resultReg, z))
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case TIMES_OP(RESULT(code1, x), RESULT(code2, y)) => {
              val code = Code(mov(eax, x), mov(resultReg, y), mul(resultReg), mov(resultReg, eax))
              RESULT(code2 ++: Seq(code), resultReg)
            }
            case ADD_OP(x, IMM(y))   => transform(ADD_OP(transform(x, edi), IMM(y)), resultReg)
            case MINUS_OP(x, IMM(y)) => transform(MINUS_OP(transform(x, edi), IMM(y)), resultReg)
            case TIMES_OP(x, IMM(y)) => transform(TIMES_OP(transform(x, edi), IMM(y)), resultReg)
            case ADD_OP(IMM(x), y)   => transform(ADD_OP(IMM(x), transform(y, edx)), resultReg)
            case MINUS_OP(IMM(x), y) => transform(MINUS_OP(IMM(x), transform(y, edx)), resultReg)
            case TIMES_OP(IMM(x), y) => transform(TIMES_OP(IMM(x), transform(y, edx)), resultReg)
            case ADD_OP(x, y)        => transform(ADD_OP(transform(x, edi), transform(y, edx)), resultReg)
            case MINUS_OP(x, y)      => transform(MINUS_OP(transform(x, edi), transform(y, edx)), resultReg)
            case TIMES_OP(x, y)      => transform(TIMES_OP(transform(x, edi), transform(y, edx)), resultReg)
            case _                   => if (inst != null) println(inst); null
          }
        }

        val result = parseAll(expr, expression).get
        val transformed = transform(result, ebp)

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

  def getCodeString(app: AsmProgram[_]): List[String] = {

    val codeSections = app.sections.collect { case x: AsmProgram[_]#CodeSection => x }
    val codeTokens = codeSections(0).builder.toList.map(_.toString)
    codeTokens.toList
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