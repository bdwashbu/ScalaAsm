package com.scalaAsm.x86
package Instructions
package Standard

object LEAVE extends InstructionDefinition[OneOpcode]("LEAVE") {
    implicit object lea1 extends _0 {
        def opcode = 0xC9
    }
}