package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Instructions.Standard._

 object Catalog {
    trait Standard {
      object callNear extends OneOperand[CALL.type]
      object add extends NewTwoOperands[ADD.type]
      object or extends NewTwoOperands[OR.type]
      object sub extends NewTwoOperands[SUB.type]
      object mul extends OneOperand[MUL.type]
      object cmp extends NewTwoOperands[CMP.type]
      
      object push extends OneOperand[PUSH.type]
      object pushf extends ZeroOperands[PUSHF.type]
      object pop extends OneOperand[POP.type]
      object dec extends NewOneOperand[DEC.type]
      object not extends NewOneOperand[NOT.type]
      object jmp extends OneOperand[JMP.type]
      object jnz extends OneOperand[JNZ.type]
      object jz extends OneOperand[JZ.type]
      object int extends OneOperand[INT.type]
      object rdrand extends OneOperand[RDRAND]
      object and extends NewTwoOperands[AND.type]
      object lea extends TwoOperands[LEA.type]
      object mov extends TwoOperands[MOV.type]
      object shr extends NewTwoOperands[SHR.type]
      object shl extends NewTwoOperands[SHL.type] with NewOneOperand[SHL.type]
      object sbb extends NewTwoOperands[SBB.type]
      object test extends TwoOperands[TEST.type]
      object xor extends NewTwoOperands[XOR.type]
      
      object retn extends ZeroOperands[RET.type] with OneOperand[RETN.type]
      object leave extends ZeroOperands[LEAVE.type]
    }
  }