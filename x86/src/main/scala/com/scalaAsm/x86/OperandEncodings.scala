package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

trait OperandFormat {
  def getAddressingForm: Option[AddressingFormSpecifier]
}

object OperandEncoding {
  
  class NoOperand extends OperandFormat {
    def getAddressingForm = None
  }
  
  abstract class OneOperand[X](x:X) extends OperandFormat {
     val operand1 = x
  }
  
  abstract class TwoOperands[X,Y](x:X, y:Y) extends OperandFormat {
     val operand1 = x
     val operand2 = y
  }
}
