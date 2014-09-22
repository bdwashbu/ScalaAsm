package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

abstract class PUSHF_1 extends ZeroOperandInstruction[OneOpcode]("PUSHF")

object PUSHF_1 {
    
  implicit object pushf1 extends PUSHF_1 {
      def opcode = 0x9C
  }
}