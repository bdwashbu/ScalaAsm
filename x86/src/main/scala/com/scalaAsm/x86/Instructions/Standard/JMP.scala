package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.Operands.Memory._

object JMP extends InstructionDefinition[OneOpcode]("JMP") with JMPImpl

// Jump

trait JMPLow {
  implicit object JMP_255_rm16 extends JMP._1[rm16] {
    def opcode = 0xFF /+ 4
  }

  implicit object JMP_255_rm32 extends JMP._1[rm32] {
    def opcode = 0xFF /+ 4
  }

  implicit object JMP_255_rm64 extends JMP._1[rm64] {
    def opcode = 0xFF /+ 4
  }
}

trait JMPImpl extends JMPLow {
  implicit object JMP_233_rel16 extends JMP._1[rel16] {
    def opcode = 0xE9
  }

  implicit object JMP_233_rel32 extends JMP._1[rel32] {
    def opcode = 0xE9
  }

  implicit object JMP_235_rel8 extends JMP._1[rel8] {
    def opcode = 0xEB
  }
}
