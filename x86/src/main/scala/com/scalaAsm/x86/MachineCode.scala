package com.scalaAsm.x86

trait MachineCode {
  val code: Array[Byte]
  val size: Int
  val line: String
  override def toString = line
}

abstract class MachineCodeBuilder1[-O1](operand1: O1) {
  def get: MachineCode
}

abstract class MachineCodeBuilder2[-O1, -O2](operand1: O1, operand2: O2) {
  def get: MachineCode
}