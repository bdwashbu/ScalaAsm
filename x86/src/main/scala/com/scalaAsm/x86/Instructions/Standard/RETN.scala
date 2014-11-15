package com.scalaAsm.x86
package Instructions
package Standard

import com.scalaAsm.x86.Operands._
import scala.annotation.implicitNotFound

object RETN extends InstructionDefinition[OneOpcode]("RETN") with RetnLow
 
trait RetnLow {
  
  implicit object retn1 extends RETN._1[imm16, I] {
      def opcode = 0xC2
  }
}