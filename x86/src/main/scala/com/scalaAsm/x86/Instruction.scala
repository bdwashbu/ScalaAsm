package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.x86Registers._



abstract class x86Instruction(mnemonic: String) extends InstructionFormat {
  implicit def toByte(x:Int) = x.toByte
  implicit def toOneOpcode(x:Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x:(Int,Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
}

trait Instruction {
  val opcode: Opcodes
  val operands: OperandFormat
  val mnemonic: String = ""
  val size: Int
    
  lazy val getBytes: Array[Byte] = {
    opcode.get ++ (operands.getAddressingForm match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
}

trait InstructionFormat extends OperandEncoding {
  def opcode: Opcodes
  def operands: OperandFormat
  val mnemonic: String = ""
    
  def build: Instruction = 
    new Instruction {
      val opcode = InstructionFormat.this.opcode
      val operands = InstructionFormat.this.operands
      override val mnemonic = InstructionFormat.this.mnemonic
      val size = getSize
    }

  override def toString = {
    mnemonic + " " + operands.toString
  }
  
  def getSize: Int = {
    opcode.size + (operands.getAddressingForm match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }
}
