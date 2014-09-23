package com.scalaAsm.x86.Operands
package Memory

import com.scalaAsm.x86._

trait AddressingMode[Size <: OperandSize] extends RegisterOrMemory[Size] 

trait AbsoluteAddress[Size <: OperandSize] extends AddressingMode[Size] with Operand[AbsoluteAddress[Size], AbsoluteAddress[Size]] {
  self =>
  var offset: Size#primitiveType
  def getRelative: Relative[Size]
  def displacement: Constant[Size]
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






trait Relative[S <: OperandSize] extends RegisterOrMemory[S] with Operand[Relative[S], Relative[S]]{
  self =>
    def displacement: Constant[S]
    def get = this
}

trait Relative32 extends Relative[_32]
trait Relative64 extends Relative[_64]