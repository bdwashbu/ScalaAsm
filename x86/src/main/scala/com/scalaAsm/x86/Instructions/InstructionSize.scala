package com.scalaAsm.x86
package Instructions

import Operands.Constant
import Operands.Register32
import Operands.UniformByteRegister

trait ModRMState
trait HasModRM extends ModRMState
trait NoMod extends ModRMState

trait SIBState
trait HasSIB extends SIBState
trait NoSIB extends SIBState

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
  
  implicit object size3 extends InstructionSize[UniformByteRegister, imm8] {
    def getSize = 3//new InstructionSizeFormat[HasModRM, NoSIB, imm8]{}
  }
  
  
}
