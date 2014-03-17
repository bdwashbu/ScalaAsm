package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.x86Registers._

trait TwoOperands[-O1,-O2] {
  protected[this] var x: O1 = _
  protected[this] var y: O2 = _
  def set(op1:O1, op2:O2) = {
    x = op1
    y = op2
  }
  def getInstruction: Instruction
}

trait OneOperand[-O1] {
  protected[this] var x: O1 = _
  def set(op1:O1) = {
    x = op1
  }
  def getInstruction: Instruction
}

abstract class x86Instruction(val mnemonic: String) {
  implicit def toByte(x:Int) = x.toByte
  implicit def toOneOpcode(x:Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x:(Int,Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
  
  def getInstruction: Instruction = 
    new Instruction {
      val opcode = x86Instruction.this.opcode
      val operands = x86Instruction.this.operands
      override val mnemonic = x86Instruction.this.mnemonic
    }
  
  def opcode: Opcodes
  def operands: OperandFormat
  def destinationReg: Option[Register] = None
}

trait Instruction extends OperandEncoding {
  val opcode: Opcodes
  val operands: OperandFormat
  val mnemonic: String = ""

  override def toString = {
    mnemonic + " " + operands.toString
  }
  
  lazy val getBytes: Array[Byte] = {
    opcode.get ++ (operands.getAddressingForm match {
      case Some(modRM) => modRM.getBytes
      case _ => Array.emptyByteArray
    })
  }
  
  lazy val size: Int = {
    opcode.size + (operands.getAddressingForm match {
      case Some(modRM) => modRM.size
      case _ => 0
    })
  }
}

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
