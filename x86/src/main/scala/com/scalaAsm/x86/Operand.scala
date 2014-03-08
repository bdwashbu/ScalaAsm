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
  val reg: Register
  val isMemory: Boolean
  val offset: Option[Immediate]
  
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

object Operands {
  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32
  
  type rm = RegisterOrMemory
  type rm8 = rm { type Size = ByteOperand }
  type rm16 = rm { type Size = WordOperand }
  type rm32 = rm { type Size = DwordOperand }
  type rm64 = rm { type Size = QwordOperand }
  
  type r8 = Register8 
  type r16 = Register16
  type r32 = Register32
  type r64 = Register64
  
  trait One
}



