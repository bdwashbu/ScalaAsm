package com.scalaAsm

import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.RegisterIndirect
package object x86 {
  
  import Operands._
  import Operands.Memory.Relative
  import Operands.Memory.AddressingMode

  trait Sized[-X] {
    val size: Int
  }
  
  trait OperandSizes {
    implicit object size7 extends Sized[Relative[DwordOperand]] {val size = 4}
    implicit object size8 extends Sized[AbsoluteAddress[_32]] {val size = 4}
    implicit object size9 extends Sized[Register[_32]] {val size = 4}
    implicit object size10 extends Sized[Constant8] {val size = 1}
    implicit object size12 extends Sized[Register[_64]] {val size = 8}
    implicit object size13 extends Sized[RegisterIndirect[_64]] {val size = 8}
    implicit object size21 extends Sized[RegisterIndirect[_32]] {val size = 4}
    implicit object size14 extends Sized[Register[_8]] {val size = 1}
    implicit object size15 extends Sized[SegmentRegister] {val size = 2}
    implicit object size16 extends Sized[GeneralPurpose[_64]#BI[_8]] {val size = 1}
    implicit object size17 extends Sized[GeneralPurpose[_32]#BI[_8]] {val size = 1}
    implicit object size19 extends Sized[GeneralPurpose[_32]#BI[_32]] {val size = 4}
    implicit object size20 extends Sized[One] {val size = 1}
  }

  // Many of these can be found in section 3.1.1.3 of the Intel x86 manual
  
  type imm = Constant[_ <: OperandSize]
  type imm8 = Constant[_8]
  type imm16 = Constant[_16]
  type imm32 = Constant[_32]
  type imm64 = Constant[_64]
  
  type rm = RegisterOrMemory[_ <: OperandSize]
  type rm8 = RegisterOrMemory[_8]
  type rm16 = RegisterOrMemory[_16]
  type rm32 = RegisterOrMemory[_32]
  type rm64 = RegisterOrMemory[_64]
  
  type m16 = AddressingMode[_16]
  
  sealed trait OperandSize {
    type primitiveType
  }
  
  type ByteOperand = OperandSize { type primitiveType = Byte }
  type WordOperand = OperandSize { type primitiveType = Short }
  type DwordOperand = OperandSize { type primitiveType = Int }
  type QwordOperand = OperandSize { type primitiveType = Long }
  
  type _8 = ByteOperand
  type _16 = WordOperand
  type _32 = DwordOperand
  type _64 = QwordOperand
  
  type r = GPR
  type r8 = GeneralPurpose[_8] with rm8
  type r16 = GeneralPurpose[_16] with rm16
  type r32 = GeneralPurpose[_32] with rm32
  type r64 = GeneralPurpose[_64] with rm64
  
  type BaseIndex[Z <: GeneralPurpose[_], Y <: OperandSize] = Z#BI[Y]
}