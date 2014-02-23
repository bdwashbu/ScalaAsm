package com.scalaAsm.x86

import x86Registers._



sealed class OperandSize {
  type OpType
}

class ByteOperand extends OperandSize { type OpType = Byte }
class WordOperand extends OperandSize { type OpType = Short }
class DwordOperand extends OperandSize { type OpType = Int }

trait RegisterOrMemory {
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
  
  type rm8 = RegisterOrMemory { type Size = ByteOperand }
  type rm16 = RegisterOrMemory { type Size = WordOperand }
  type rm32 = RegisterOrMemory { type Size = DwordOperand }
  
  type r8 = Register8 
  type r16 = Register16
  type r32 = Register32
  
  trait One
}



