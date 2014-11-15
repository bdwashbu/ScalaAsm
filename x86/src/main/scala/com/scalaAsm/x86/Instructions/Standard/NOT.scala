package com.scalaAsm.x86
package Instructions
package Standard

object NOT extends InstructionDefinition[OneOpcode]("NOT") with notLow
 
trait notLow {
  
  implicit object not1 extends NOT._1[rm32, M] {
      def opcode = 0xF7 /+ 2
  }
}