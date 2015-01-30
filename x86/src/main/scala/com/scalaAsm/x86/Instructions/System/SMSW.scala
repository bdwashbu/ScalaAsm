package com.scalaAsm.x86
package Instructions
package System

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

// Description: Store Machine Status Word
// Category: general

object SMSW extends InstructionDefinition[OneOpcode]("SMSW") with SMSWImpl

trait SMSWImpl {
  implicit object SMSW_0 extends SMSW._1[m16] {
    def opcode = 0x1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SMSW_1 extends SMSW._1[r16] {
    def opcode = 0x1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SMSW_2 extends SMSW._1[r32] {
    def opcode = 0x1 /+ 4
    override def hasImplicitOperand = true
  }

  implicit object SMSW_3 extends SMSW._1[r64] {
    def opcode = 0x1 /+ 4
    override def prefix = REX.W(true)
    override def hasImplicitOperand = true
  }
}
