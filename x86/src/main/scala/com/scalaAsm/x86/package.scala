package com.scalaAsm

import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect
package object x86 {
  
  import Operands._

  trait Sized[-X] {
    val size: Int
  }
  
  trait OperandSizes {
    implicit object size7 extends Sized[Relative{type Size = DwordOperand}] {val size = 4}
    implicit object size8 extends Sized[AbsoluteAddress[Constant32]] {val size = 4}
    implicit object size9 extends Sized[Register32] {val size = 4}
    implicit object size10 extends Sized[Constant8] {val size = 1}
    implicit object size11 extends Sized[BaseIndex[EBP, Constant8]] {val size = 1}
    implicit object size12 extends Sized[Register64] {val size = 8}
    implicit object size13 extends Sized[RegisterIndirect[RSP]] {val size = 8}
    implicit object size14 extends Sized[Register8] {val size = 1}
    implicit object size15 extends Sized[SegmentRegister] {val size = 2}
    implicit object size16 extends Sized[BaseIndex[RSP, Constant8]] {val size = 1}
    implicit object size17 extends Sized[BaseIndex[ESP, Constant8]] {val size = 1}
    implicit object size18 extends Sized[BaseIndex[EAX, Constant8]] {val size = 1}
    implicit object size19 extends Sized[BaseIndex[EDI, Constant32]] {val size = 4}
    implicit object size20 extends Sized[One] {val size = 1}
  }
  
  import Operands.Memory.Relative
  import Operands.Memory.AddressingMode
  
  trait Operands
  case class TwoOperands[+X, +Y](_1: X, _2: Y) extends Operands
  case class OneOperand[+X](_1: X) extends Operands
  
  type imm8 = Constant8
  type imm16 = Constant16
  type imm32 = Constant32
  type imm64 = Constant64
  
  type Operand = com.scalaAsm.x86.Operands.Operand
  
  //type AddressingFormSpecifier = Operands.AddressingFormSpecifier
  
  type rm = RegisterOrMemory
  type rm8 = rm { type Size = ByteOperand }
  type rm16 = rm { type Size = WordOperand }
  type rm32 = rm { type Size = DwordOperand }
  type rm64 = rm { type Size = QwordOperand }
  
  type m16 = AddressingMode { type Size = WordOperand }
  
  
  
  type r = GPR
  type r8 = Register8 with GeneralPurpose
  type r16 = Register16 with GeneralPurpose
  type r32 = Register32 with GeneralPurpose
  type r64 = Register64 with GeneralPurpose
  
    type BaseIndex[X <: GeneralPurpose,Y <: Constant[_]] = X#BI[Y]
}