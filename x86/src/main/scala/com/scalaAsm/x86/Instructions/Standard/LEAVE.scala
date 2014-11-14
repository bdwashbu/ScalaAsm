package com.scalaAsm.x86
package Instructions
package Standard

object LEAVE extends OperandInstruction[OneOpcode]("LEAVE") {
    implicit object lea1 extends ZeroOps {
        def opcode = 0xC9
    }
}