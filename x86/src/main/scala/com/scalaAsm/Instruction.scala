package com.scalaAsm.x86

//import com.scalaAsm.x86.ModRMFormat._

 protected[x86] trait AddressingFormSpecifier extends ModRMFormat with Operands {
    val modRM: ModRMByte
    val scaleIndexBase: Option[Byte]
    val displacment: Option[Byte]
    val immediate: Option[Byte]
  }

private[x86] trait Instruction extends Operands {
  
//  trait Instruction1 {
//	val opcode: Byte
//	val addressingFormSpecifier: AddressingFormSpecifier
//  }
}

private[x86] case class Instruction1(opcode: Byte, addressingFormSpecifier: AddressingFormSpecifier) extends ModRMFormat with Operands

private[x86] trait Instruction2 extends ModRMFormat with Operands {
	val opcode: (Byte, Byte)
	val addressingFormSpecifier: AddressingFormSpecifier
}