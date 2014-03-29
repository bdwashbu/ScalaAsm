package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.ModRM

trait OpcodeFormat {
  def size: Int
  def get(x: Any): Array[Byte]
  val opcodeExtension: Option[Byte]
  def /+ (x: Byte): OpcodeFormat
}

