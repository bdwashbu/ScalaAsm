package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode extends RegisterOrMemory

trait ImmediateMemory extends AddressingMode {
  self =>
  def immediate: Immediate { type Size = self.Size}
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtend.get, new EBP))
  }
  
  def rel32: Relative32 = new Relative32 {
    def offset = new Displacement32{ val value = self.immediate.asInt}
    def size = 4
  }
  
  def rel64: Relative64 = new Relative64 {
    def offset = new Displacement64{ val value = self.immediate.asLong}
    def size = 8
  }
}

trait RegisterIndirect extends AddressingMode {
  self =>
  def base: GPR

  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIBNoDisplacement(ModRMReg(NoDisplacement, reg, rm = base))
  }
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
  }
}


trait BaseIndex extends AddressingMode {
  self =>
  def base: GPR
  def offset: Displacement

  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    (base, offset) match {
      case (base: Register64, _) =>
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtend.get, base), SIB(SIB.One, new ESP, base))
      case (base, _: Displacement8) =>
        NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcodeExtend.get, base), offset)
      //case (base, None) =>
       // NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
      case _ => NoModRM()
    }
  }
  
//  override def toString = {
//    var result: String = ""
//    
//    result = "[" + base.toString
//    if (offset.isDefined) {
//      if (!offset.get.isNegative)
//    	  result += " + " + offset.get.toString
//      else
//    	  result += " - " + offset.get.negate.toString
//    }
//    result += "]"
//    
//    result
//  }
}