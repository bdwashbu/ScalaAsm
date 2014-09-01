package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode[Size <: OperandSize] extends RegisterOrMemory[Size] 

trait AbsoluteAddress[C <: Constant[C]] extends AddressingMode[C#Size] with Operand[AbsoluteAddress[C], AbsoluteAddress[C]]{
  self =>
  var offset: C#Size#primitiveType
  def getRelative: Relative[C#Size]
  def displacement: C
  def get = this
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

abstract class RegisterIndirect[X <: OperandSize](reg: GeneralPurpose[X]) extends AddressingMode[X]  with Operand[RegisterIndirect[X], RegisterIndirect[X]]{
  self =>
  def base: GeneralPurpose[X] = reg
  def get = this
//  def encode(reg: GPR, opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//    OnlyModRM(ModRMReg(NoDisplacement, reg, rm = base))
//  }
  
//  def encode(opcodeExtend: Option[Byte]): AddressingFormSpecifier = {
//    OnlyModRM(ModRMOpcode(NoDisplacement, opcodeExtend.get, base))
//  }
}




trait Relative[S <: OperandSize] extends RegisterOrMemory[S] with Operand[Relative[S], Relative[S]]{
  self =>
    def displacement: Constant[_] {type Size = S}
    def get = this
}

trait Relative32 extends Relative[_32]
trait Relative64 extends Relative[_64]