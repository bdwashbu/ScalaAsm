package com.scalaAsm.x86
package Operands

trait Operand

trait DualOperand[-O1,-O2] extends Operand {
  self: Instruction =>
  protected[this] var x: O1 = _
  protected[this] var y: O2 = _
  def set(op1:O1, op2:O2) = {
    x = op1
    y = op2
  }
}

trait SingleOperand[-O1] extends Operand {
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
  def base: Option[GPR]
  def offset: Option[Immediate]
  
  def rel32: Relative32 = new Relative32 {
    def offset = Memory.this.offset
  }
  
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

trait Relative extends RegisterOrMemory {
    def offset: Option[Immediate]
}

trait Relative32 extends Relative {
  type Size = DwordOperand
}

trait RegisterOrMemory extends Any {
  type Size <: OperandSize
}



