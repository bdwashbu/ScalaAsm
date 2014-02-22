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

 
  
}
