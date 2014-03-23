package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Instruction

trait AsmCodeSimple extends Registers {
    
    implicit val builder = new SimpleCodeBuilder{}
    
    def imm8(x: Byte) = Immediate8(x)
    def imm16(x: Short) = Immediate16(x)
    def imm32(x: Int) = Immediate32(x)
    
    def byte(x: Byte) = Displacement8(x)
    def word(x: Short) = Displacement16(x)
    def dword(x: Int) = Displacement32(x)
    
    def getRawBytes: Array[Byte] = {
      builder.codeTokens.flatMap(x => x.code).toArray
    }

    implicit def toByte(x: Int) = x.toByte
    val One = new One{}
    
    def twoOps[O1,O2](p1:O1,p2:O2, ev: Instruction with TwoOperands[O1,O2], code: SimpleCodeBuilder) = {
      ev.set(p1,p2)
      code.codeTokens += ev.build
    }
    
    def oneOp[O1](p1:O1, ev: Instruction with OneOperand[O1], code: SimpleCodeBuilder) = {
      ev.set(p1)
      code.codeTokens += ev.build
    }
    
    
    def sub[O1, O2](p1: O1, p2: O2)(implicit ev: SUB_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)  
    
    def add[O1, O2](p1: O1, p2: O2)(implicit ev: ADD_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def push[O1](p1: O1)(implicit ev: PUSH_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)

    def pop[O1](p1: O1)(implicit ev: POP_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)
      
    def dec[O1](p1: O1)(implicit ev: DEC_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)

    def and[O1, O2](p1: O1, p2: O2)(implicit ev: AND_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def not[O1](p1: O1)(implicit ev: NOT_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)

    def lea[O1, O2](p1: O1, p2: O2)(implicit ev: LEA_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def mov[O1, O2](p1: O1, p2: O2)(implicit ev: MOV_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def shr[O1, O2](p1: O1, p2: O2)(implicit ev: SHR_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)
    
    def jnz[O1](p1: O1)(implicit ev: JNZ_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)
    
    def jz[O1](p1: O1)(implicit ev: JZ_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)
    
    def int[O1](p1: O1)(implicit ev: INT_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)

    def shl[O1, O2](p1: O1, p2: O2)(implicit ev: SHL_2[O1,O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def sbb[O1, O2](p1: O1, p2: O2)(implicit ev: SBB_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def retn[O1](p1: O1)(implicit ev: RETN_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)

    def retn(implicit ev: RET, code: SimpleCodeBuilder) = code.codeTokens += ev.build

    def test[O1, O2](p1: O1, p2: O2)(implicit ev: TEST_2[O1, O2], code: SimpleCodeBuilder) = twoOps(p1,p2,ev,code)

    def leave(implicit ev: LEAVE, code: SimpleCodeBuilder) = code.codeTokens += ev.build

    def rdrand[O1](p1: O1)(implicit ev: RDRAND_1[O1], code: SimpleCodeBuilder) = oneOp(p1,ev,code)
}

trait AsmCode extends Registers {
  self: AsmProgram =>

  val code: Code
  
  def proc(name: String)(x: => Unit)(implicit code: CodeBuilder) = {
    val result = Procedure(name)
    code.codeTokens += result
    x
  }

  case class Code() {
    
    implicit val builder = new CodeBuilder{}
    
    def imm8(x: Byte) = Immediate8(x)
    def imm16(x: Short) = Immediate16(x)
    def imm32(x: Int) = Immediate32(x)
    
    def byte(x: Byte) = Displacement8(x)
    def word(x: Short) = Displacement16(x)
    def dword(x: Int) = Displacement32(x)

    var parserPos: Int = 0

    implicit def toByte(x: Int) = x.toByte
    val One = new One{}

    def twoOps[O1,O2](p1:O1,p2:O2, ev: Instruction with TwoOperands[O1,O2], code: CodeBuilder) = {
      ev.set(p1,p2)
      code.codeTokens += CodeToken(ev.build)
    }
    
    def oneOp[O1](p1:O1, ev: Instruction with OneOperand[O1], code: CodeBuilder) = {
      ev.set(p1)
      code.codeTokens += CodeToken(ev.build)
    }
    
    private def procRef(procName: String)(implicit code: CodeBuilder) =
      code.codeTokens += ProcRef(procName)

    def label(name: String)(implicit code: CodeBuilder) =
       code.codeTokens += Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte)(implicit code: CodeBuilder) =
       code.codeTokens += Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String)(implicit code: CodeBuilder) =
      code.codeTokens += Reference(param)

    def sub[O1, O2](p1: O1, p2: O2)(implicit ev: SUB_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code) 
      
    def add[O1, O2](p1: O1, p2: O2)(implicit ev: ADD_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def mul[O1](p1: O1)(implicit ev: MUL_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)
    
    def push[O1](p1: O1)(implicit ev: PUSH_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)

    def pop[O1](p1: O1)(implicit ev: POP_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)
      
    def dec[O1](p1: O1)(implicit ev: DEC_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)

    def and[O1, O2](p1: O1, p2: O2)(implicit ev: AND_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def not[O1](p1: O1)(implicit ev: NOT_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)

    def lea[O1, O2](p1: O1, p2: O2)(implicit ev: LEA_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def mov[O1, O2](p1: O1, p2: O2)(implicit ev: MOV_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def shr[O1, O2](p1: O1, p2: O2)(implicit ev: SHR_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def jnz(labelRef: String)(implicit code: CodeBuilder) =
      code.codeTokens += LabelRef(labelRef, 0x75.toByte)
    
    def jnz[O1](p1: O1)(implicit ev: JNZ_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)

    def jz(labelRef: String)(implicit code: CodeBuilder) =
      code.codeTokens += LabelRef(labelRef, 0x74.toByte)
    
    def jz[O1](p1: O1)(implicit ev: JZ_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)
    
    def int[O1](p1: O1)(implicit ev: INT_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)

    def shl[O1, O2](p1: O1, p2: O2)(implicit ev: SHL_2[O1,O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def sbb[O1, O2](p1: O1, p2: O2)(implicit ev: SBB_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def retn[O1](p1: O1)(implicit ev: RETN_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)

    def retn(implicit ev: RET, code: CodeBuilder) = code.codeTokens += CodeToken(ev.build)

    def test[O1, O2](p1: O1, p2: O2)(implicit ev: TEST_2[O1, O2], code: CodeBuilder) = twoOps(p1,p2,ev,code)

    def leave(implicit ev: LEAVE, code: CodeBuilder) = code.codeTokens += CodeToken(ev.build)

    def call(refName: String)(implicit code: CodeBuilder) =
      code.codeTokens += Reference(refName)

    def jmp(ref: String)(implicit code: CodeBuilder) =
      code.codeTokens += JmpRef(ref)
      
    def rdrand[O1](p1: O1)(implicit ev: RDRAND_1[O1], code: CodeBuilder) = oneOp(p1,ev,code)
  }

}