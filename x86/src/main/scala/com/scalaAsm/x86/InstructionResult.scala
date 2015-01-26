package com.scalaAsm.x86

trait InstructionResult extends Function0[Array[Byte]] {
  def mnemonic: String
  def apply: Array[Byte]
  override def toString = mnemonic
  def getSize: Int = apply.size
}