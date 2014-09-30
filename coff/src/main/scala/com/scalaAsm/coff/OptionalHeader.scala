package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder

object OptionalHeader {
  def getOptionalHeader(input: ByteBuffer): OptionalHeader = {
    val magic = input.getShort()
    val header = OptionalHeader(
      magic = magic,
      majorLinkerVersion = input.get(),
      minorLinkerVersion = input.get(),
      sizeOfCode = input.getInt(),
      sizeOfInitializedData = input.getInt(),
      sizeOfUninitData = input.getInt(),
      addressOfEntryPoint = input.getInt(),
      baseOfCode = input.getInt(),
      baseOfData = input.getInt()
    )
    input.order(ByteOrder.LITTLE_ENDIAN)

    header
  }
}

case class OptionalHeader(
  magic: Short,
  majorLinkerVersion: Byte,
  minorLinkerVersion: Byte,
  sizeOfCode: Int,
  sizeOfInitializedData: Int,
  sizeOfUninitData: Int,
  addressOfEntryPoint: Int,
  baseOfCode: Int,
  baseOfData: Int) {

  def apply() = {
    val bbuf = ByteBuffer.allocate(200)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.putShort(magic)
    bbuf.put(majorLinkerVersion.toByte)
    bbuf.put(minorLinkerVersion.toByte)
    bbuf.putInt(sizeOfCode)
    bbuf.putInt(sizeOfInitializedData)
    bbuf.putInt(sizeOfUninitData)
    bbuf.putInt(addressOfEntryPoint)
    bbuf.putInt(baseOfCode)
    bbuf.putInt(baseOfData)
    bbuf.array().take(bbuf.position())
  }
}