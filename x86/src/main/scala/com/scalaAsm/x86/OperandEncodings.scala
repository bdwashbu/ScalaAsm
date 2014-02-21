package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat

object OperandEncoding {
  
  class OneOperand[X](x:X) extends OperandFormat {
     val operand1 = x
  }
  
  class TwoOperands[X,Y](x:X, y:Y) extends OperandFormat {
     val operand1 = x
     val operand2 = y
  }

  case class MI[M <: ModRM.reg, I <: imm](op1: M, op2: I) extends TwoOperands[M, I](op1, op2)
  
  case class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends TwoOperands[R,M](op1, op2)
  
  case class O[R <: ModRM.reg](op1: R) extends OneOperand[R](op1)
  
  case class I[I <: Immediate](op1: I) extends OneOperand[I](op1)
  
  case class M[M <: ModRM.rm](op1: M) extends OneOperand[M](op1)
  
  case class M1[M <: ModRM.rm](op1: M) extends OneOperand[M](op1)
  
}
