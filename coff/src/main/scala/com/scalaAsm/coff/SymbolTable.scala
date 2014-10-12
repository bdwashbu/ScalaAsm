package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder

case class CoffSymbol(name: String, location: Int)

object SymbolEntry {
  def getSymbolEntry(input: ByteBuffer): SymbolEntry = {
     val name = Array.fill(8)(0.toByte)
     input.get(name)
     SymbolEntry(
         name = name map(_.toChar) mkString,
         value = input.getInt(),
         sectionNumber   = input.getShort(),
         symbolType    = input.getShort(),
         storageClass = input.get(),
         auxillaryCount    = input.get()
     )
  }
}

case class SymbolEntry (
    name: String,
    value: Int,
    sectionNumber: Short,
    symbolType: Short,
    storageClass: Byte,
    auxillaryCount: Byte) {
  
  def apply() = {
    val bbuf = ByteBuffer.allocate(18)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(name.toCharArray() map (_.toByte))
    bbuf.putInt(value)
    bbuf.putShort(sectionNumber)
    bbuf.putShort(symbolType)
    bbuf.put(storageClass)
    bbuf.put(auxillaryCount)
    bbuf.array()
  }
  
}