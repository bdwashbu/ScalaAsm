package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.ModRM

trait Opcodes {
  def get: Array[Byte]
  def size: Int
  val opcodeExtension: Option[Byte]
  def /+ (x: Byte): Opcodes
}

case class OpcodePlusRd(operand1:Byte, reg: ModRM.reg) extends Opcodes {
  def get = Array((operand1 + reg.ID).toByte)
  val size = 1
  val opcodeExtension: Option[Byte] = None
  def /+ (x: Byte) = new OneOpcode(operand1) { override val opcodeExtension = Some(x) }
}

case class OneOpcode(operand1:Byte) extends Opcodes {
  def get = Array(operand1)
  val size = 1
  val opcodeExtension: Option[Byte] = None
  def /+ (x: Byte) = new OneOpcode(operand1) { override val opcodeExtension = Some(x) }
}

case class TwoOpcodes(operand1:Byte, operand2:Byte) extends Opcodes {
  def get = Array(operand1, operand2)
  val size = 2
  val opcodeExtension: Option[Byte] = None
  def /+ (x: Byte) = new TwoOpcodes(operand1, operand2) { override val opcodeExtension = Some(x) }
}