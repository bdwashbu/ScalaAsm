package com.scalaAsm.x86

trait MachineCode {
  val code: Array[Byte]
  val size: Int
  val line: String
  override def toString = line
}