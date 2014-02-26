package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.x86Registers._
import com.scalaAsm.x86.Instruction
import com.scalaAsm.x86.OneOperand
import com.scalaAsm.x86.OperandEncoding
import com.scalaAsm.x86.{ModRM, x86Instruction, OperandSize, Opcodes, OneOpcode, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

abstract class DEC extends x86Instruction("DEC")

trait DEC_1[-O1] extends DEC with OneOperand[O1] with OperandEncoding

object DEC {
  
  implicit object dec1 extends DEC_1[r32] {
     def operands = O(x)
     def opcode: Opcodes = 0x48 + x.ID
  }
  
  implicit object dec2 extends DEC_1[r16] {
     def operands = O(x)
	 def opcode: Opcodes = 0x48 + x.ID
  }
}