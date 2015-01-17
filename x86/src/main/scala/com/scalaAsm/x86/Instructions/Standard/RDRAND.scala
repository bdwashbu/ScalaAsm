package com.scalaAsm.x86
package Instructions
package Standard

import scala.annotation.implicitNotFound

class RDRAND extends InstructionDefinition[TwoOpcodes]("RDRAND") {
 
  trait rdrandImpl {
    
    implicit object RDRAND_1 extends _1[rm32] {
        def opcode = (0x0F, 0xC7) /+ 6
    }
    
    implicit object RDRAND_2 extends _1[rm16] {
        def opcode = (0x0F, 0xC7) /+ 6
    }
  }
}