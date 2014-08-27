package com.scalaAsm.x86

trait InstructionResult {
  def getBytes: Array[Byte]
  def getSize: Int
  def line: String
  override def toString = line
}