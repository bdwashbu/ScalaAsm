package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions
import com.scalaAsm.x86.Operands
import com.scalaAsm.asm.Tokens._

trait AsmCode extends Registers with Instructions with Operands {
  self: AsmProgram =>

  val code: Code
  
  case class Proc(name: String)(implicit code: CodeBuilder) {
      implicit val builder = new CodeBuilder{}
      code.codeTokens += this
  }

  case class Code  {
    
    implicit val builder = new CodeBuilder{}

    def b(bytes: Byte*): Array[Byte] = {
      bytes.toArray
    }

    var parserPos: Int = 0

    implicit def toByte(x: Int) = x.toByte

//    private def toCode(machineCode: Array[Byte]) = Code {
//      codeTokens :+ CodeToken(machineCode, machineCode.size)
//    }

    private def procRef(procName: String)(implicit code: CodeBuilder) =
      code.codeTokens += ProcRef(procName)

    def label(name: String)(implicit code: CodeBuilder) =
       code.codeTokens += Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte)(implicit code: CodeBuilder) = {
       code.codeTokens += Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)
    }

    def push(param: String)(implicit code: CodeBuilder) =
      code.codeTokens += Reference(param)

    def add[O1, O2](p1: O1, p2: O2)(implicit ev: ADD_2[O1, O2], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1, p2))

//    def push[O1 <: Register[_]](p1: O1)(implicit ev: PUSH_O[O1]) = toCode {
//      ev.get(p1)
//    }

    def push[O1](p1: O1)(implicit ev: PUSH_M[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def pop[O1](p1: O1)(implicit ev: POP_O[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def and[O1, O2](p1: O1, p2: O2)(implicit ev: AND_RM[O1, O2], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1, p2))

    def not[O1](p1: O1)(implicit ev: NOT_M[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def lea[O1, O2, O3](p1: O1, p2: O2, p3: O3)(implicit ev: LEA_3[O1, O2, O3], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get)

    def lea[O1, O2](p1: O1, p2: O2)(implicit ev: LEA_2[O1, O2], code: CodeBuilder) = 
      code.codeTokens += CodeToken(ev.get(p1, p2))

    def mov[O1, O2, O3](p1: O1, p2: O2, p3: O3)(implicit ev: MOV_RM[O1, O2, O3], code: CodeBuilder) = 
      code.codeTokens += CodeToken(ev.get(p3))

    def mov[O1, O2](p1: O1, p2: O2)(implicit ev: MOV_R[O1, O2], code: CodeBuilder) = 
      code.codeTokens += CodeToken(ev.get(p1, p2))

    def mov[O1, O2](p1: O1)(implicit ev: MOV_R2[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def shr[O1, O2](p1: O1, p2: O2)(implicit ev: SHR_2[O1, O2], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1, p2))

    def jnz(labelRef: String)(implicit code: CodeBuilder) =
      code.codeTokens += LabelRef(labelRef, 0x75.toByte)
    
    def jnz[O1](p1: O1)(implicit ev: JNZ_1[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def jz(labelRef: String)(implicit code: CodeBuilder) =
      code.codeTokens += LabelRef(labelRef, 0x74.toByte)
    
    def jz[O1](p1: O1)(implicit ev: JZ_1[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))
    
    def int[O1](p1: O1)(implicit ev: INT_1[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def shl[O1](p1: O1)(implicit ev: SHL_1[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def sbb[O1, O2](p1: O1, p2: O2)(implicit ev: SBB_2[O1, O2], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1, p2))

    def retn[O1](p1: O1)(implicit ev: RETN_1[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def retn(implicit code: CodeBuilder) = code.codeTokens += CodeToken(Array(0xC3))

    def test[O1](p1: O1)(implicit ev: TEST_1[O1], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1))

    def test[O1, O2](p1: O1, p2: O2)(implicit ev: TEST_2[O1, O2], code: CodeBuilder) =
      code.codeTokens += CodeToken(ev.get(p1, p2))

    def leave(implicit code: CodeBuilder) = code.codeTokens += CodeToken(Array(0xC9))

    def call(refName: String)(implicit code: CodeBuilder) =
      code.codeTokens += Reference(refName)

    def jmp(ref: String)(implicit code: CodeBuilder) =
      code.codeTokens += JmpRef(ref)
  }

}