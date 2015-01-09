package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._

object JMP extends InstructionDefinition[OneOpcode]("JMP") with JMPImpl

trait JMPImpl {
  implicit object JMP_233_rel16 extends JMP._1_new[rel16] {
    def opcode = 0xE9
  }

  implicit object JMP_233_rel32 extends JMP._1_new[rel32] {
    def opcode = 0xE9
  }

  implicit object JMP_235_rel8 extends JMP._1_new[rel8] {
    def opcode = 0xEB
  }

  implicit object JMP_255_rm16 extends JMP._1_new[rm16] {
    def opcode = 0xFF /+ 4
  }

  implicit object JMP_255_rm32 extends JMP._1_new[rm32] {
    def opcode = 0xFF /+ 4
  }
}
