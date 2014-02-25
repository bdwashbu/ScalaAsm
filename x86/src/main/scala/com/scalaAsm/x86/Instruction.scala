package com.scalaAsm.x86

import com.scalaAsm.x86.ModRM._
import com.scalaAsm.x86.Operands._

abstract class x86Instruction(val mnemonic: String) {
  implicit def toByte(x:Int) = x.toByte
  implicit def toOneOpcode(x:Int): OneOpcode = OneOpcode(x.toByte)
  implicit def toTwoOpcodes(x:(Int,Int)): TwoOpcodes = TwoOpcodes(x._1.toByte, x._2.toByte)
  
  trait Instruction extends OperandEncoding {
	  val opcode: Opcodes
	  val operands: OperandFormat
	
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
}



trait Opcodes {
  def get: Array[Byte]
  def size: Int
  val opcodeExtension: Option[Byte]
  def /+ (x: Byte): Opcodes
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
