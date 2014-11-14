package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object PUSHF extends OperandInstruction[OneOpcode]("PUSHF") with pushfLow

trait pushfLow {
  implicit object pushf1 extends PUSHF.ZeroOps {
        def opcode = 0x9C
    }
}