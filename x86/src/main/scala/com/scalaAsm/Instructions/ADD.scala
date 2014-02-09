package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86._
import com.scalaAsm.x86.AddressingFormSpecifier
import x86Registers._

trait ADD extends Instruction with Operands

trait ADD_2[-O1, -O2] extends ADD {
  def get(op1: O1, op2: O2): Instruction1
  def getBytes(op1: O1, op2: O2): Array[Byte] = {
    val blah = get(op1, op2)
    Array(blah.opcode) ++ blah.addressingFormSpecifier.getBytes
  }
  def getAddressForm(op1: O1, op2: O2): AddressingFormSpecifier
}

trait MI[X <: OperandSize] extends ADD_2[ModeRMFormat.reg[X], imm8] {
  def getAddressForm(op1: ModeRMFormat.reg[X], op2: imm8): AddressingFormSpecifier = new AddressingFormSpecifier {
        type immType = imm8
        val modRM: ModRMByte = ModRMExtended(Register, 0.toByte, op1)
	    val scaleIndexBase: Option[Byte] = None
	    val displacment: Option[Byte] = None
	    val immediate: Option[immType] = Some(op2)
  }
}
trait MR[X <: OperandSize] extends ADD_2[ModeRMFormat.rm[X], ModeRMFormat.reg[X]]

object ADD extends Instruction {
  implicit object add1 extends MI[DwordOperand] {
    def get(x: r32, y: imm8) = {
	    new Instruction1( 
	      opcode = 0x83.toByte,
	      opcodeExtension = 0.toByte,
		  addressingFormSpecifier = getAddressForm(x,y)
	     )
     }
  }
}