package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode extends RegisterOrMemory

trait AbsoluteAddress extends AddressingMode {
  self =>
  def displacement: Constant { type Size = self.Size}
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    NoSIBWithDisplacement(ModRMOpcode(NoDisplacement, opcodeExtend.get, new EBP), displacement)
  }
  
  def rel32: Relative32 = new Relative32 {
    def displacement = new Constant32{ val value = self.displacement.asInt}
    def size = 4
  }
  
  def rel64: Relative64 = new Relative64 {
    def displacement = new Constant64{ val value = self.displacement.asLong}
    def size = 8
  }
}

trait RegisterIndirect extends AddressingMode {
  self =>
  def base: GPR

  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    OnlyModRM(ModRMReg(NoDisplacement, reg, rm = base))
  }
  
  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
  }
}


trait BaseIndex extends AddressingMode {
  self =>
  def base: GPR
  def displacement: Constant

  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
    (base, displacement) match {
      case (base: Register64, _) =>
        WithSIBNoDisplacement(ModRMOpcode(NoDisplacement, opcodeExtend.get, base), SIB(SIB.One, new ESP, base))
      case (base, _: Constant8) =>
        NoSIBWithDisplacement(ModRMOpcode(DisplacementByte, opcodeExtend.get, base), displacement)
        
      //case (base, None) =>
       // NoSIB(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
      case _ => NoModRM()
    }
  }
  
  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
	    (base, displacement) match {
	      case (base, off: Constant8) if base.ID == 4 =>
	        WithSIBWithDisplacement(ModRMReg(DisplacementByte, reg, base), SIB(SIB.One, new ESP, base), displacement)
	      case (base, off: Constant32) =>
	        NoSIBWithDisplacement(ModRMReg(DisplacementDword, reg = reg, rm = base), displacement)
	      case (base, _: Constant) =>
	        NoSIBWithDisplacement(ModRMReg(DisplacementByte, reg = reg, rm = base), displacement)
	      //case (base, None) =>
	       // NoSIB(ModRMReg(NoDisplacement, reg = this, rm = base))
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

trait Relative extends RegisterOrMemory {
  self =>
    def displacement: Constant {type Size = self.Size}
    
    def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = OnlyDisplacement(displacement)
}

trait Relative32 extends Relative {
  type Size = DwordOperand
}

trait Relative64 extends Relative {
  type Size = QwordOperand
}