package com.scalaAsm.x86

trait OpcodeFormat {
  def size: Int
  def get(x: Any): Array[Byte]
  val opcodeExtension: Option[Byte]
  def /+ (x: Byte): OpcodeFormat
}

