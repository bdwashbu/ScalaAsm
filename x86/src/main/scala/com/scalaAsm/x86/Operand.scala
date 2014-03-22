package com.scalaAsm.x86

import x86Registers._

trait Operands

trait TwoOperands[-O1,-O2] extends Operands {
  self: Instruction =>
  protected[this] var x: O1 = _
  protected[this] var y: O2 = _
  def set(op1:O1, op2:O2) = {
    x = op1
    y = op2
  }
}

trait OneOperand[-O1] extends Operands {
  self: Instruction =>
  protected[this] var x: O1 = _
  def set(op1:O1) = {
    x = op1
  }
}

sealed class OperandSize {
  type size
}

class ByteOperand extends OperandSize { type size = Byte }
class WordOperand extends OperandSize { type size = Short }
class DwordOperand extends OperandSize { type size = Int }
class QwordOperand extends OperandSize { type size = Long }

trait Memory extends RegisterOrMemory {
  type Size <: OperandSize
  def base: GPR
  def offset: Option[Immediate]
  
  override def toString = {
    var result: String = ""
    
    result = "[" + base.toString
    if (offset.isDefined) {
      if (!offset.get.isNegative)
    	  result += " + " + offset.get.toString
      else
    	  result += " - " + offset.get.negate.toString
    }
    result += "]"
    
    result
  }
}

trait RegisterOrMemory extends Any {
  type Size <: OperandSize
}



