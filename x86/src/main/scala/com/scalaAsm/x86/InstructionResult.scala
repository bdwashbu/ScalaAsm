package com.scalaAsm.x86

trait InstructionResult {
  def mnemonic: String
  def getBytes: Array[Byte]
  override def toString = mnemonic
  def getSize: Int = getBytes.size
}