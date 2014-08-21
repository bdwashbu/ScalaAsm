package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode extends RegisterOrMemory

trait AbsoluteAddress[C <: Constant[C]] extends AddressingMode {
  self =>
  type Size = C#Size
  var offset: Size#primitiveType
  def getRelative: Relative[C#Size]
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

abstract class RegisterIndirect[-Reg <: GPR](reg: Reg) extends AddressingMode {
  self =>
  def base: GPR = reg
//  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//    OnlyModRM(ModRMReg(NoDisplacement, reg, rm = base))
//  }
  
//  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//    OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
//  }
}




trait Relative[S <: OperandSize] extends RegisterOrMemory {
  self =>
    def displacement: Constant[_] {type Size = S}
}

trait Relative32 extends Relative[_32]
trait Relative64 extends Relative[_64]