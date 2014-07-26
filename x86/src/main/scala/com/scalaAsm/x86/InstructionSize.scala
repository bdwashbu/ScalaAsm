package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands.Register32
import com.scalaAsm.x86.Operands.UniformByteRegister

trait ModRMState
trait HasModRM extends ModRMState
trait NoMod extends ModRMState

trait SIBState
trait HasSIB extends SIBState
trait NoSIB extends SIBState

trait Sized[Field] extends SizedInstructionField

trait InstructionSize[-X <: Operand, -Y <: Operand] {
  def getSize: Int //InstructionSizeFormat[_,_,_]
}

trait Sizes {
  implicit object size1 extends InstructionSize[r32, imm8] {
    def getSize = 3//new InstructionSizeFormat[HasModRM, NoSIB, imm8]{}
  }
  
  implicit object size2 extends InstructionSize[r32, r32] {
    def getSize = 2//new InstructionSizeFormat[HasModRM, NoSIB, imm8]{}
  }
  
  implicit object size3 extends InstructionSize[UniformByteRegister[_], imm8] {
    def getSize = 3//new InstructionSizeFormat[HasModRM, NoSIB, imm8]{}
  }
  
  
}

trait InstructionSizeFormat[ModRM <: ModRMState, SIB <: SIBState, C <: Constant[C]] {
  def size(implicit mod: Sized[ModRM], sib: Sized[SIB], constant: Sized[C]): Int = mod.size + sib.size + constant.size
}