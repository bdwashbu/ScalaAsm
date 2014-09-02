package com.scalaAsm.x86.Instructions

package object Standard {
  
  trait Catalog {
    object callNear extends testOneOperand[CALL_1]
    object add extends testTwoOperands[ADD_2]
    object or extends testTwoOperands[OR_2]
    object sub extends testTwoOperands[SUB_2]
    object mul extends testOneOperand[MUL_1]
    
    object push extends testOneOperand[PUSH_1]
    object pop extends testOneOperand[POP_1]
    object dec extends testOneOperand[DEC_1]
    object not extends testOneOperand[NOT_1]
    object jmp extends testOneOperand[JMP_1]
    object jnz extends testOneOperand[JNZ_1]
    object jz extends testOneOperand[JZ_1]
    object int extends testOneOperand[INT_1]
    object rdrand extends testOneOperand[RDRAND_1]
    object and extends testTwoOperands[AND_2]
    object lea extends testTwoOperands[LEA_2]
    object mov extends testTwoOperands[Mov2Def]
    object shr extends testTwoOperands[SHR_2]
    object shl extends testTwoOperands[SHL_2]
    object sbb extends testTwoOperands[SBB_2]
    object test extends testTwoOperands[TEST_2]
    object xor extends testTwoOperands[XOR_2]
    
    object retn extends testZeroOperands[RET] with testOneOperand[RETN_1]
    object leave extends testZeroOperands[LEAVE]
  }
}