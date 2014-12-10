package com.play_interface

import java.io._
import scala.util.parsing.combinator.JavaTokenParsers
import com.scalaAsm.asm.Tokens.Variable
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.x86.Operands._
import scala.collection.mutable.HashSet
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.Tokens.CodeToken
import com.scalaAsm.asm.{ x86_32, x86_64 }
import com.scalaAsm.x86._

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

        def transform(inst: Inst, resultReg: Operand[r32], availableRegs: Set[Operand[r32]]): RESULT = {

          def recurse(inst: Inst, availableRegs: Set[Operand[r32]]): RESULT = {
           println(inst)
            inst match {
              // these end up being evaluated last
              case ADD_OP(IMM(x), IMM(y)) => {
                val code = Code(mov(availableRegs.head, dword(x.toInt)), add(availableRegs.head, byte(y.toInt)))
                RESULT(Seq(code), availableRegs.head)
              }
              case MINUS_OP(IMM(x), IMM(y)) => {
                val code = Code(mov(availableRegs.head, dword(x.toInt)), sub(availableRegs.head, byte(y.toInt)))
                RESULT(Seq(code), availableRegs.head)
              }
              case TIMES_OP(IMM(x), IMM(y)) => {
                val eaxIsTaken = availableRegs - eax
                val code = Code(push(eax), mov(eax, dword(x.toInt)), mov(eaxIsTaken.head, dword(y.toInt)), mul(eaxIsTaken.head), mov(eaxIsTaken.head, eax), pop(eax))
                RESULT(Seq(code), availableRegs.head)
              }
              // answer propagation
              case ADD_OP(IMM(x), RESULT(code2, y)) => {
                val available = availableRegs - y
                val code = Code(mov(available.head, dword(x.toInt)), add(available.head, y))
                RESULT(code2 ++: Seq(code), available.head)
              }
              case ADD_OP(RESULT(code2, y), IMM(x)) => {
                val available = availableRegs - y
                val code = Code(mov(available.head, dword(x.toInt)), add(available.head, y))
                RESULT(code2 ++: Seq(code), available.head)
              }
              case MINUS_OP(IMM(x), RESULT(code2, y)) => {
                val available = availableRegs - y
                val code = Code(mov(available.head, dword(x.toInt)), sub(available.head, y))
                RESULT(code2 ++: Seq(code), available.head)
              }
              case MINUS_OP(RESULT(code2, x), IMM(z)) => {
                val available = availableRegs - x
                val code = Code(mov(available.head, x), sub(available.head, byte(z.toInt)))
                RESULT(code2 ++: Seq(code), available.head)
              }
              case TIMES_OP(IMM(x), RESULT(code2, y)) => {
                val eaxIsTaken = availableRegs - eax - y
                val code = Code(push(eax), mov(eax, dword(x.toInt)), mov(eaxIsTaken.head, y), mul(eaxIsTaken.head), mov(eaxIsTaken.head, eax), pop(eax))
                RESULT(code2 ++: Seq(code), eaxIsTaken.head)
              }
              case TIMES_OP(RESULT(code2, y), IMM(x)) => {
                val eaxIsTaken = availableRegs - eax - y
                val code = Code(push(eax), mov(eax, dword(x.toInt)), mov(eaxIsTaken.head, y), mul(eaxIsTaken.head), mov(eaxIsTaken.head, eax), pop(eax))
                RESULT(code2 ++: Seq(code), eaxIsTaken.head)
              }
              case ADD_OP(RESULT(code1, x), RESULT(code2, z)) => {
                val available = availableRegs - x - z
                val code = Code(mov(available.head, x), add(available.head, z))
                RESULT(code1 ++: code2 ++: Seq(code), available.head)
              }
              case MINUS_OP(RESULT(code1, x), RESULT(code2, z)) => {
                val available = availableRegs - x - z
                val code = Code(mov(available.head, x), sub(available.head, z))
                RESULT(code1 ++: code2 ++: Seq(code), available.head)
              }
              case TIMES_OP(RESULT(code1, x), RESULT(code2, y)) => {
                val eaxIsTaken = availableRegs - eax - x - y
                val code = Code(push(eax), mov(eax, x), mov(eaxIsTaken.head, y), mul(eaxIsTaken.head), mov(eaxIsTaken.head, eax), pop(eax))
                RESULT(code1 ++: code2 ++: Seq(code), eaxIsTaken.head)
              }
              case ADD_OP(x, IMM(y))   => recurse(ADD_OP(recurse(x, availableRegs), IMM(y)), availableRegs)
              case MINUS_OP(x, IMM(y)) => recurse(MINUS_OP(recurse(x, availableRegs), IMM(y)), availableRegs)
              case TIMES_OP(x, IMM(y)) => recurse(TIMES_OP(recurse(x, availableRegs), IMM(y)), availableRegs)
              case ADD_OP(IMM(x), y)   => recurse(ADD_OP(IMM(x), recurse(y, availableRegs)), availableRegs)
              case MINUS_OP(IMM(x), y) => recurse(MINUS_OP(IMM(x), recurse(y, availableRegs)), availableRegs)
              case TIMES_OP(IMM(x), y) => recurse(TIMES_OP(IMM(x), recurse(y, availableRegs)), availableRegs)
              // these are hit first, assign regs
              case ADD_OP(x, y)        => {
                val leftResult = recurse(x, availableRegs)
                val rightResult = recurse(y, availableRegs - leftResult.reg)
                recurse(ADD_OP(leftResult, rightResult), availableRegs)
              }
              case MINUS_OP(x, y)      => {
                val leftResult = recurse(x, availableRegs)
                val rightResult = recurse(y, availableRegs - leftResult.reg)
                recurse(MINUS_OP(leftResult, rightResult), availableRegs)
              }
              case TIMES_OP(x, y)      => {
                val leftResult = recurse(x, availableRegs)
                val rightResult = recurse(y, availableRegs - leftResult.reg)
                recurse(TIMES_OP(leftResult, rightResult), availableRegs)
              }
              case _                   => if (inst != null) println(inst); null
            }
          }
          val result = recurse(inst, availableRegs)
          val code = Code(mov(resultReg, result.reg))
          RESULT(result.code ++: Seq(code), resultReg)
        }

        val result = parseAll(expr, expression).get
        val transformed = transform(result, ebp, Set(edi, ebx, ecx, edx))

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