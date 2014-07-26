package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode extends RegisterOrMemory

trait AbsoluteAddress[C <: Constant[C]] extends AddressingMode {
  self =>
  type Size = C#Size
  var offset: Size#primitiveType
  def getRelative: Relative { type Size = self.Size }
  def displacement: C
}
//trait AbsoluteAddress32 extends AbsoluteAddress {
//  self =>
//  def displacement: Constant { type Size = self.Size}
//    
//  def getRelative = new Relative32 {
//    def displacement = Constant32(self.displacement.asInt)
//    def size = 4
//  }
//}
//
//trait AbsoluteAddress64 extends AbsoluteAddress {
//  self =>
//  def displacement: Constant { type Size = self.Size}
//
//  def getRelative = new Relative64 {
//    def displacement = Constant64(self.displacement.asLong)
//    def size = 8
//  }
//}

abstract class RegisterIndirect[-Reg <: GPR[_]](reg: Reg) extends AddressingMode {
  self =>
  def base: GPR[_] = reg
//  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//    OnlyModRM(ModRMReg(NoDisplacement, reg, rm = base))
//  }
  
//  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//    OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
//  }
}


abstract class BaseIndex[-Reg <: GPR[_], -Disp <: Constant[_]](reg: Reg, disp: Disp) extends AddressingMode {
  self =>
  def base: GPR[_] = reg
  def displacement: Constant[_] = disp
  
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
    def displacement: Constant[_] {type Size = self.type#Size}
}

trait Relative32 extends Relative {
  type Size = DwordOperand
}

trait Relative64 extends Relative {
  type Size = QwordOperand
}