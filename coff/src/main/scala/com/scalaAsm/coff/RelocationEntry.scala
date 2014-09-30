package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder

case class RelocationEntry(
    referenceAddress: Long,
    symbolIndex: Long,
    reloationType: Short) {
  
  def apply() = {
    val bbuf = ByteBuffer.allocate(10)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.putLong(referenceAddress)
    bbuf.putLong(symbolIndex)
    bbuf.putShort(reloationType)
    bbuf.array()
  }
  
}