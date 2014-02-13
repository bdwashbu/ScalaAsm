package com.scalaAsm.x86

//import com.scalaAsm.x86.ModRMFormat._

 protected[x86] trait AddressingFormSpecifier extends ModRM with Operands {
    val modRM: ModRMFormat
    val scaleIndexBase: Option[Byte]
    val displacment: Option[Byte]
    val immediate: Option[Immediate]
    
    def getBytes: Array[Byte] = {
      Array(modRM.get) ++ immediate.get.getBytes
    }
  }

private[x86] trait Instruction extends Operands {
  
//  trait Instruction1 {
//	val opcode: Byte
//	val addressingFormSpecifier: AddressingFormSpecifier
//  }
}

private[x86] case class Instruction1(opcode: Byte, addressingFormSpecifier: AddressingFormSpecifier = null, opcodeExtension: Byte = -1) extends ModRM with Operands

//private[x86] trait Instruction2 extends ModRMFormat with Operands {
//	val opcode: (Byte, Byte)
//	val addressingFormSpecifier: AddressingFormSpecifier
//}