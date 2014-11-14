package com.scalaAsm.x86
package Instructions
package Standard

object NOT extends OperandInstruction[OneOpcode]("NOT") with notLow
 
trait notLow {
  
  implicit object not1 extends NOT.OneOp[rm32, M] {
      def opcode = 0xF7 /+ 2
  }
}