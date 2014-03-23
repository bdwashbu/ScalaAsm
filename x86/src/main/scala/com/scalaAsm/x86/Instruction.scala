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

trait Instruction {
  def opcode: Opcodes
  def operands: OperandFormat
  val mnemonic: String = ""
    
  def build: MachineCode = 
    new MachineCode {
      val size = getSize
      val code = getBytes
      val line = mnemonic + " " + operands.toString
    }

  def getSize: Int = {
    opcode.size + (operands.getAddressingForm match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }
  
   def getBytes: Array[Byte] = {
    opcode.get ++ (operands.getAddressingForm match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}
