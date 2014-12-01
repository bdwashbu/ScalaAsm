package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder

case class CoffSymbol(name: String, location: Int, sectionNumber: Short, symbolType: SymbolType, storageClass: StorageClass, auxillary: Seq[AuxiliarySymbol]) {
  override def toString = {
    "CoffSymbol(\"" + name + "\", " + location + ", " + sectionNumber + ')'
  }
  
  def apply() = {
    import scala.language.implicitConversions
    val bbuf = ByteBuffer.allocate(18)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    if (name.length >= 8) {
      bbuf.putInt(0)
      bbuf.putInt(0)
    } else {
      bbuf.put(name.toCharArray() map (_.toByte))
    }
    bbuf.putInt(0)
    bbuf.putShort(sectionNumber)
    bbuf.putShort(symbolType.value)
    bbuf.put(storageClass.value)
    bbuf.array()
  }
}

sealed abstract class SymbolType(val value: Short)
case object IMAGE_SYM_DTYPE_NULL extends SymbolType(0)
case object IMAGE_SYM_DTYPE_POINTER extends SymbolType(1)
case object IMAGE_SYM_DTYPE_FUNCTION extends SymbolType(2)
case object IMAGE_SYM_DTYPE_ARRAY extends SymbolType(3)

object SymbolType {
  def apply(value: Short): SymbolType = {
   val dtype = value & 0x00FF
    dtype match {
    case 0 => IMAGE_SYM_DTYPE_NULL
    case 1 => IMAGE_SYM_DTYPE_POINTER
    case 2 => IMAGE_SYM_DTYPE_FUNCTION
    case 3 => IMAGE_SYM_DTYPE_ARRAY
  }}
}

sealed abstract class StorageClass(val value: Byte)
case object IMAGE_SYM_CLASS_NULL extends StorageClass(0)
case object IMAGE_SYM_CLASS_AUTOMATIC extends StorageClass(1)
case object IMAGE_SYM_CLASS_EXTERNAL extends StorageClass(2)
case object IMAGE_SYM_CLASS_STATIC extends StorageClass(3)
case object IMAGE_SYM_CLASS_FILE extends StorageClass(103)

object StorageClass {
  def apply(value: Byte): StorageClass = value match {
    case 0 => IMAGE_SYM_CLASS_NULL
    case 1 => IMAGE_SYM_CLASS_AUTOMATIC
    case 2 => IMAGE_SYM_CLASS_EXTERNAL
    case 3 => IMAGE_SYM_CLASS_STATIC
    case 103 => IMAGE_SYM_CLASS_FILE
  }
}

object StandardSymbolEntry {
  def getSymbolEntry(input: ByteBuffer): StandardSymbolEntry = {
     val rawName = Array.fill(8)(0.toByte)
     input.get(rawName)
     val name = rawName map(_.toChar) mkString
     val value = input.getInt()
     val sectionNumber = input.getShort()
     val symbolType = SymbolType(input.getShort())
     val storageClass = StorageClass(input.get())
     val numAuxiliary = input.get()
     StandardSymbolEntry(
         name,
         value,
         sectionNumber,
         symbolType,
         storageClass,
         auxiliarySymbols = for (i <- 0 until numAuxiliary) yield {
           storageClass match {
             case IMAGE_SYM_CLASS_STATIC => AuxiliarySectionDefinition.getAuxSectionDef(input)
             case IMAGE_SYM_CLASS_FILE => AuxiliaryFormatFiles.getAuxFormatFiles(input)
             case _ => null
           }
         }
     )
  }
}

trait SymbolEntry 

case class StandardSymbolEntry (
    name: String,
    value: Int,
    sectionNumber: Short,
    symbolType: SymbolType,
    storageClass: StorageClass,
    auxiliarySymbols: Seq[AuxiliarySymbol]) extends SymbolEntry {
  
  def apply() = {
    import scala.language.implicitConversions
    val bbuf = ByteBuffer.allocate(18)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(name.toCharArray() map (_.toByte))
    bbuf.putInt(value)
    bbuf.putShort(sectionNumber)
    bbuf.putShort(symbolType.value)
    bbuf.put(storageClass.value)
    bbuf.put(auxiliarySymbols.size.toByte)
    bbuf.array()
  }
  
  def isStringReference = name(0) == '\u0000' && name(1) == '\u0000' && name(2) == '\u0000' && name(3) == '\u0000'
    
  def getStringOffset = {
    val bbuf2 = ByteBuffer.allocate(4)
    bbuf2.order(ByteOrder.LITTLE_ENDIAN)
    bbuf2.put(name(4).toByte)
    bbuf2.put(name(5).toByte)
    bbuf2.put(name(6).toByte)
    bbuf2.put(name(7).toByte)
    bbuf2.rewind()
    bbuf2.getInt()
  }
}

trait AuxiliarySymbol extends SymbolEntry

// For symbol entries with a storage class of 3
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
     (0 until 3).foreach{x => input.get()} // toss out padding
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

// For symbol entries with a storage class of 103 (0x67)
object AuxiliaryFormatFiles {
   def getAuxFormatFiles(input: ByteBuffer): AuxiliaryFormatFiles = {
     
     val stringData: Array[Byte] = Array.fill(18)(0);
     input.get(stringData)
     AuxiliaryFormatFiles(
         new String(stringData).split('\u0000')(0)
     )
  }
}

case class AuxiliaryFormatFiles (
    fileName: String) extends AuxiliarySymbol {
  
  def apply() = {
    val bbuf = ByteBuffer.allocate(18)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(fileName.padTo(18, 0.toChar) map(_.toByte) toArray)
    bbuf.array()
  } 
}