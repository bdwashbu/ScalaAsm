package com.scalaAsm.x86
package Instructions
package Standard

import scala.annotation.implicitNotFound

class RDRAND extends InstructionDefinition[TwoOpcodes]("RDRAND") {
 
  object RDRAND_1 {
    
    implicit object rdrand1 extends _1[rm32, M] {
        def opcode = (0x0F, 0xC7) /+ 6
    }
    
    implicit object rdrand2 extends _1[rm16, M] {
        def opcode = (0x0F, 0xC7) /+ 6
    }
  }
}