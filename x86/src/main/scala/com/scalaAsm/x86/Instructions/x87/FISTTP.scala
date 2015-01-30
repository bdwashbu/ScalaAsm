package com.scalaAsm.x86
package Instructions
package x87

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Integer with Truncation and Pop
// Category: general/conver

object FISTTP extends InstructionDefinition[OneOpcode]("FISTTP") with FISTTPImpl

trait FISTTPImpl {
  implicit object FISTTP_0 extends FISTTP._1[m32] {
    def opcode = 0xDB /+ 1
    override def hasImplicitOperand = true
  }

  implicit object FISTTP_1 extends FISTTP._1[m16] {
    def opcode = 0xDF /+ 1
    override def hasImplicitOperand = true
  }
}
