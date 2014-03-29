package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

abstract class x86Instruction(mnemonic: String) extends Instruction {
  implicit def toByte(x:Int) = x.toByte
  implicit def toOneOpcode(x:Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x:(Int,Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait InstructionField extends Any {
  def getBytes: Array[Byte]
}

trait Instruction

trait ZeroOperandInstruction extends Instruction {
  def opcode: OpcodeFormat
  def operands: NoOperand
  val mnemonic: String = ""
    
  def build: MachineCode = 
    new MachineCode {
      val size = getSize
      val code = getBytes
      val line = mnemonic + " " + operands.toString
    }

  def getSize: Int = {
    0
  }
  
   def getBytes: Array[Byte] = {
    Array()
  }
}

trait OneOperandInstruction[-O1] extends SingleOperand[O1] with Instruction {
  def opcode: OpcodeFormat
  def operands: OneOperandFormat[O1]
  val mnemonic: String = ""
    
  def build(x:O1): MachineCode = 
    new MachineCode {
      val size = getSize(x)
      val code = getBytes(x)
      val line = mnemonic + " " + operands.toString
    }

  def getSize(x:O1): Int = {
    opcode.size + (operands.getAddressingForm(x,opcode) match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }
  
   def getBytes(x:O1): Array[Byte] = {
    opcode.get(x) ++ (operands.getAddressingForm(x,opcode) match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}

trait TwoOperandInstruction[-O1,-O2] extends DualOperand[O1,O2] with Instruction {
  def opcode: OpcodeFormat
  def operands: TwoOperandsFormat[O1,O2]
  val mnemonic: String = ""
    
  def build(x:O1,y:O2): MachineCode = 
    new MachineCode {
      val size = getSize(x,y)
      val code = getBytes(x,y)
      val line = mnemonic + " " + operands.toString
    }

  def getSize(x:O1,y:O2): Int = {
    opcode.size + (operands.getAddressingForm(x,y,opcode) match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }
  
   def getBytes(x:O1,y:O2): Array[Byte] = {
    opcode.get ++ (operands.getAddressingForm(x,y,opcode) match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}
