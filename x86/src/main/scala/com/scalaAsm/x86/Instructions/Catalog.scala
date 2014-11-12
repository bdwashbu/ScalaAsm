package com.scalaAsm.x86
package Instructions

import com.scalaAsm.x86.Instructions.Standard._

 object Catalog {
    trait Standard {
      object callNear extends OneOperand[CALL_1]
      object add extends TwoOperands[ADD_2]
      object or extends TwoOperands[OR_2]
      object sub extends TwoOperands[SUB_2]
      object mul extends OneOperand[MUL_1]
      
      object push extends OneOperand[PUSH_1]
      object pushf extends ZeroOperands[PUSHF_1]
      object pop extends OneOperand[POP_1]
      object dec extends OneOperand[DEC_1]
      object not extends OneOperand[NOT_1]
      object jmp extends OneOperand[JMP_1]
      object jnz extends OneOperand[JNZ_1]
      object jz extends OneOperand[JZ_1]
      object int extends OneOperand[INT_1]
      object rdrand extends OneOperand[RDRAND_1]
      object and extends TwoOperands[AND_2]
      object lea extends TwoOperands[LEA_2]
      object mov extends TwoOperands[MOV_2]
      object shr extends TwoOperands[SHR_2]
      object shl extends TwoOperands[SHL_2]
      object sbb extends TwoOperands[SBB_2]
      object test extends TwoOperands[TEST_2]
      object xor extends TwoOperands[XOR_2]
      
      object retn extends ZeroOperands[RET] with OneOperand[RETN_1]
      object leave extends ZeroOperands[LEAVE]
    }
  }