package com.scalaAsm.x86

import x86Registers._

sealed class OperandSize {
  type size
}

class ByteOperand extends OperandSize { type size = Byte }
class WordOperand extends OperandSize { type size = Short }
class DwordOperand extends OperandSize { type size = Int }
class QwordOperand extends OperandSize { type size = Long }

trait RegisterOrMemory extends Any {
  type Size <: OperandSize
  def reg: GPR
  def isMemory: Boolean
  def offset: Option[Immediate]
  
  override def toString = {
    var result: String = ""
    if (isMemory) {
      result = "[" + reg.toString
      if (offset.isDefined) {
          if (!offset.get.isNegative)
        	  result += " + " + offset.get.toString
          else
        	  result += " - " + offset.get.negate.toString
      }
      result += "]"
    } else {
      reg.toString
    }
    
    result
  }
}



