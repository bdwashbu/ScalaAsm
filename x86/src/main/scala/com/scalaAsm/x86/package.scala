package com.scalaAsm

import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress
import com.scalaAsm.x86.Operands.Memory.Memory

package object x86 {
  
  import Operands._

  // These can be found in section 3.1.1.3 of the Intel x86 manual
  
  type imm = Constant[_]
  type imm8 = Constant[_8]
  type imm16 = Constant[_16]
  type imm32 = Constant[_32]
  type imm64 = Constant[_64]
  
  type rm = RegisterOrMemory[_]
  type rm8 = RegisterOrMemory[_8]
  type rm16 = RegisterOrMemory[_16]
  type rm32 = RegisterOrMemory[_32]
  type rm64 = RegisterOrMemory[_64]
  
  type m = Memory[_]
  type m8 = Memory[_8]
  type m16 = Memory[_16]
  type m32 = Memory[_32]
  type m64 = Memory[_64]
  type m128 = Memory[_128]

  type _8 = Byte
  type _16 = Short
  type _32 = Int
  type _64 = Long
  type _128 = Long // fix
  
  type reg = GPR
  type r8 = GeneralPurpose[_8]
  type r16 = GeneralPurpose[_16]
  type r32 = GeneralPurpose[_32]
  type r64 = GeneralPurpose[_64]
  
  type Sreg = SegmentRegister
  
  type moffs8 = AbsoluteAddress[_8]
  type moffs16 = AbsoluteAddress[_16]
  type moffs32 = AbsoluteAddress[_32]
  type moffs64 = AbsoluteAddress[_64]
  
  type rel = Constant[_]
  type rel8 = Constant[_8]
  type rel16 = Constant[_16]
  type rel32 = Constant[_32]
  type rel64 = Constant[_64]
  
  type +[Reg <: GeneralPurpose[_], Y] = Reg#BaseIndex[Y]
}