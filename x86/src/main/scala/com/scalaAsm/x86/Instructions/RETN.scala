package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.{ModRM, Instruction, OperandSize, OneOpcode, Immediate, DwordOperand, WordOperand}
import com.scalaAsm.x86.AddressingFormSpecifier
import com.scalaAsm.x86.x86Registers._
import scala.annotation.implicitNotFound
import com.scalaAsm.utils.Endian

trait RETN extends ModRM
trait RETN_1[O1] extends RETN {
  def get(p1: O1): Array[Byte]
}

object RETN {
  implicit object retn1 extends RETN_1[imm16] { def get(x: imm16) = Array(0xC2.toByte) ++ Endian.swap(x.value) }
}