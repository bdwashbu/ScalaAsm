package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Instructions.Standard._

 object Catalog {
    trait Standard {
      object callNear extends NewOneOperand[CALL.type]
      object add extends NewTwoOperands[ADD.type]
      object or extends NewTwoOperands[OR.type]
      object sub extends NewTwoOperands[SUB.type]
      object mul extends NewOneOperand[MUL.type]
      object cmp extends NewTwoOperands[CMP.type]
      
      object push extends NewOneOperand[PUSH.type]
      object pushf extends NewZeroOperands[PUSHF.type]
      object pop extends NewOneOperand[POP.type]
      object dec extends NewOneOperand[DEC.type]
      object not extends NewOneOperand[NOT.type]
      object jmp extends NewOneOperand[JMP.type]
      object jnz extends NewOneOperand[JNZ.type]
      object jz extends NewOneOperand[JZ.type]
      object int extends NewOneOperand[INT.type]
      object rdrand extends OneOperand[RDRAND]
      object and extends NewTwoOperands[AND.type]
      object lea extends NewTwoOperands[LEA.type]
      object mov extends TwoOperands[MOV.type]
      object shr extends NewTwoOperands[SHR.type]
      object shl extends NewTwoOperands[SHL.type] with NewOneOperand[SHL.type]
      object sbb extends NewTwoOperands[SBB.type]
      object test extends NewTwoOperands[TEST.type]
      object xor extends NewTwoOperands[XOR.type]
      
      object retn extends NewZeroOperands[RETN.type] with NewOneOperand[RETN.type]
      object leave extends NewZeroOperands[LEAVE.type]
    }
  }