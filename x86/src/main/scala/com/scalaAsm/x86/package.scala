package com.scalaAsm

import com.scalaAsm.x86.Operands.Operand
package object x86 {
  
  trait Operands
  case class TwoOperands[+X <: Operand, +Y <: Operand](_1: X, _2: Y) extends Operands
  case class OneOperand[+X <: Operand](_1: X) extends Operands
}