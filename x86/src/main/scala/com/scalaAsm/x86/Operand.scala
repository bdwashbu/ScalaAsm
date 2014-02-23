package com.scalaAsm.x86

import x86Registers._

object Addressing {

  case class RegisterOffset[+T <: Register, S <: Immediate](offset: S, x: T)

  trait Addressable[X <: Register] {
    self: X =>
    def -[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset.negate, this)
    def +[Z <: Immediate {type X = Z }](offset: Z) = RegisterOffset[X, Z](offset, this)
  }

  case class *[+A](x: A)
  type +[A <: Register, B <: Immediate] = RegisterOffset[A, B]
}

sealed class OperandSize {
  type OpType
}
class ByteOperand extends OperandSize { type OpType = Byte }
class WordOperand extends OperandSize { type OpType = Short }
class DwordOperand extends OperandSize { type OpType = Int }

trait RegisterOrMemory {
  type Size <: OperandSize
  val reg: Register
  val isMemory: Boolean
  val offset: Option[Immediate]
}

object Operands {
  type imm8 = Immediate8
  type imm16 = Immediate16
  type imm32 = Immediate32
  
  type rm8 = RegisterOrMemory { type Size = ByteOperand }
  type rm16 = RegisterOrMemory { type Size = WordOperand }
  type rm32 = RegisterOrMemory { type Size = DwordOperand }
  
  type r8 = Register8 
  type r16 = Register16
  type r32 = Register32
  
  trait One
}



