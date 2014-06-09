package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.Memory.ModRM

trait OpcodeFormat {
  def size: Int
  def get(x: Any): Array[Byte]
  val opcodeExtension: Option[Byte]
  def /+(x: Byte): OpcodeFormat
}

sealed trait RegType
case object rw extends RegType
case object rd extends RegType
case object rb extends RegType

case class OneOpcode(operand1: Byte) extends OpcodeFormat {
  def get(x: Any) = Array(operand1)
  val size = 1
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new OneOpcode(operand1) { override val opcodeExtension = Some(x) }
  def +(reg: RegType) = OpcodePlus(operand1)
  
  private[OneOpcode] case class OpcodePlus(opcode1: Byte) extends OpcodeFormat {
	  def get(x: Any) = {
	    if (x.isInstanceOf[ModRM.reg])
	      Array((opcode1 + x.asInstanceOf[ModRM.reg].ID).toByte)
	    else
	      Array()
	
	  }
	  val size = 1
	  val opcodeExtension: Option[Byte] = None
	  def /+(x: Byte) = new OneOpcode(opcode1) { override val opcodeExtension = Some(x) }
	}
}

case class TwoOpcodes(opcode1: Byte, opcode2: Byte) extends OpcodeFormat {
  def get(x: Any) = Array(opcode1, opcode2)
  val size = 2
  val opcodeExtension: Option[Byte] = None
  def /+(x: Byte) = new TwoOpcodes(opcode1, opcode2) { override val opcodeExtension = Some(x) }
}

