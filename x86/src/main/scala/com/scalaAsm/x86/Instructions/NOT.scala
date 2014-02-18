package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, Instruction1, Instruction2, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier

trait NOT extends ModRM

trait NOT_M[-O1] extends NOT {
  def get(p: O1): Array[Byte]
}

object NOT {
  implicit object not1 extends NOT_M[rm32] { def get(x: rm32) = 0xF7.toByte +: modRMExtended(x, extensionCode = 2).getBytes }
}