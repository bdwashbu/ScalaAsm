package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands.UniformByteRegister

object OR extends InstructionDefinition[OneOpcode]("OR") with orHigh
 
trait orLow {
 implicit object or1 extends OR._2[rm8, imm8, MI] {
      def opcode = 0x80 /+ 1
  } 
}

trait orHigh {
  implicit object or2 extends OR._2[UniformByteRegister[_8], imm8, MI] {
    def opcode = 0x80 /+ 1
    override def prefix = REX(false, false, false, false)
  }
}
