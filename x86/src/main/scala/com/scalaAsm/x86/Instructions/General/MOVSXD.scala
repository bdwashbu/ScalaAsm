package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object MOVSXD extends InstructionDefinition[OneOpcode]("MOVSXD") with MOVSXDImpl

// Move with Sign-Extension

trait MOVSXDLow {
  implicit object MOVSXD_0 extends MOVSXD._2[r32, rm32] {
    def opcode = 0x63 /r
    override def explicitFormat(op1: r32, op2: rm32) = {
      if (op2.isInstanceOf[reg]) {
        Some(InstructionFormat(addressingForm = OnlyModRM(ModRMReg(TwoRegisters, op1, op2.asInstanceOf[reg])), immediate = None))
      } else None
    }
  }
}

trait MOVSXDImpl extends MOVSXDLow {
  implicit object MOVSXD_1 extends MOVSXD._2[r64, rm32] {
    def opcode = 0x63 /r
  }
}
