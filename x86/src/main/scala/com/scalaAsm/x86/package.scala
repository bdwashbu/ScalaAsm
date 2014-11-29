package com.scalaAsm

import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Memory.AbsoluteAddress

package object x86 {
  
  import Operands._
  import Operands.Memory.Relative

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

  type _8 = Byte
  type _16 = Short
  type _32 = Int
  type _64 = Long
  
  type r = GPR with rm
  type r8 = GeneralPurpose[_8] with rm8
  type r16 = GeneralPurpose[_16] with rm16
  type r32 = GeneralPurpose[_32] with rm32
  type r64 = GeneralPurpose[_64] with rm64
  
  type rel = Relative[_]
  type rel8 = Relative[_8]
  type rel16 = Relative[_16]
  type rel32 = Relative[_32]
  type rel64 = Relative[_64]
  
  type +[Reg <: GeneralPurpose[_], Y] = Reg#BaseIndex[Y]
}