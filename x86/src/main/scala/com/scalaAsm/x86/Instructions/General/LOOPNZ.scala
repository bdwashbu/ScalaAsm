package com.scalaAsm.x86
package Instructions
package General

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object LOOPNZ extends InstructionDefinition[OneOpcode]("LOOPNZ") with LOOPNZImpl

// Decrement count; Jump short if count!=0 and ZF=0

trait LOOPNZImpl {
  implicit object LOOPNZ_0 extends LOOPNZ._1[rel8] {
    def opcode = 0xE0
    override def hasImplicitOperand = true
  }
}
