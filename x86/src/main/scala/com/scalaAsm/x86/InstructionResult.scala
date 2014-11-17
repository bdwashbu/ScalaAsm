package com.scalaAsm.x86

trait InstructionResult {
  def mnemonic: String
  def getBytes: Array[Byte]
  def getSize: Int
  override def toString = mnemonic
}