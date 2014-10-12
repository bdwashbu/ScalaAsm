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
         sectionNumber = input.getShort(),
         symbolType = input.getShort(),
         storageClass = input.get(),
         auxiliarySymbols = for (i <- 0 until input.get()) yield AuxiliarySectionDefinition.getAuxSectionDef(input)
     )
  }
}

case class SymbolEntry (
    name: String,
    value: Int,
    sectionNumber: Short,
    symbolType: Short,
    storageClass: Byte,
    auxiliarySymbols: Seq[AuxiliarySymbol]) {
  
  def apply() = {
    val bbuf = ByteBuffer.allocate(18)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(name.toCharArray() map (_.toByte))
    bbuf.putInt(value)
    bbuf.putShort(sectionNumber)
    bbuf.putShort(symbolType)
    bbuf.put(storageClass)
    bbuf.put(auxiliarySymbols.size.toByte)
    bbuf.array()
  } 
}

trait AuxiliarySymbol

object AuxiliarySectionDefinition {
  def getAuxSectionDef(input: ByteBuffer): AuxiliarySectionDefinition = {
     val result = AuxiliarySectionDefinition(
         length = input.getInt(),
         numberOfRelocations = input.getShort(),
         numberOfLineNumbers = input.getShort(),
         checksum = input.getInt(),
         number = input.getShort(),
         selection = input.get()
     )
     input.get() // toss out padding
     input.get()
     input.get()
     result
  }
}

case class AuxiliarySectionDefinition (
    length: Int,
    numberOfRelocations: Short,
    numberOfLineNumbers: Short,
    checksum: Int,
    number: Short,
    selection: Byte) extends AuxiliarySymbol {
  
  def apply() = {
    val bbuf = ByteBuffer.allocate(18)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.putInt(length)
    bbuf.putShort(numberOfRelocations)
    bbuf.putShort(numberOfLineNumbers)
    bbuf.putInt(checksum)
    bbuf.putInt(number)
    bbuf.put(selection)
    bbuf.array()
  } 
}