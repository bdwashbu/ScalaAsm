package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

package object Instructions {

  trait OneOperand[X <: InstructionDefinition[_]] {
    def apply[O1](p1: Operand[O1])(implicit ev: X#_1[O1], format: OneOperandFormat[O1]) = ev(p1, format, ev.prefix)
  }
 
  trait TwoOperands[X <: InstructionDefinition[_]] {
    def apply[O1, O2](p1: Operand[O1], p2: Operand[O2])(implicit ev: X#_2[O1, O2], format: TwoOperandFormat[O1, O2]) = ev(p1,p2, format)
  }
  
  class ZeroOperands[X <: InstructionDefinition[_]] {
    def apply(ignored: Unit)(implicit ev: X#_0) = ev.get
  }
  
}