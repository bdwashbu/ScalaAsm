package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.ListBuffer

sealed abstract class SymbolType(val raw: Short, val value: Short)
case class IMAGE_SYM_DTYPE_NULL(rawValue: Short) extends SymbolType(rawValue, 0)
case class IMAGE_SYM_DTYPE_POINTER(rawValue: Short) extends SymbolType(rawValue, 1)
case class IMAGE_SYM_DTYPE_FUNCTION(rawValue: Short) extends SymbolType(rawValue, 2)
case class IMAGE_SYM_DTYPE_ARRAY(rawValue: Short) extends SymbolType(rawValue, 3)

object SymbolType {
  def apply(value: Short): SymbolType = {
   val dtype = value & 0x00FF
    dtype match {
    case 0 => IMAGE_SYM_DTYPE_NULL(value)
    case 1 => IMAGE_SYM_DTYPE_POINTER(value)
    case 2 => IMAGE_SYM_DTYPE_FUNCTION(value)
    case 3 => IMAGE_SYM_DTYPE_ARRAY(value)
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

object CoffSymbol {
  def getSymbolEntry(input: ByteBuffer): CoffSymbol = {
     val rawName = Array.fill(8)(0.toByte)
     input.get(rawName)
     val name = rawName.map(_.toChar).mkString
     val value = input.getInt()
     val sectionNumber = input.getShort()
     val symbolType = SymbolType(input.getShort())
     val storageClass = StorageClass(input.get())
     val numAuxiliary = input.get()
     CoffSymbol(
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
  
  def writeRelocationsAndSymbols(relocations: Seq[Relocation], symbols: Seq[CoffSymbol]): Array[Byte] = {
    val (longSymbols, shortSymbols) = symbols.partition(_.name.length >= 8)
    
    val tablePositions: Seq[(Int, String)] = new ListBuffer[(Int, String)]()
    
    var stringTablePos = 0
    for (string <- longSymbols.map(_.name)) {
      tablePositions ++: Seq((stringTablePos, string))
      stringTablePos += string.size
    }
    
    val posBuffer = ListBuffer[(String, Int)]()
    var currentPos = 4
    for (sym <- longSymbols) {
      posBuffer += ((sym.name, currentPos))
      currentPos += sym.name.length + 1
    }
    val stringTableMap = posBuffer.toMap
    
    val stringTable = longSymbols.map(_.name.toCharArray().map(_.toByte) ++: Array('\u0000'.toByte)).reduce(_ ++ _)
    
    val tableLength = ByteBuffer.allocate(4)
    tableLength.order(ByteOrder.LITTLE_ENDIAN)
    tableLength.putInt(stringTable.length + 4)
    
    // resolve symbols
    symbols.foreach{ sym =>
      if (sym.name.length > 8) {
        val bbuf = ByteBuffer.allocate(8)
        bbuf.order(ByteOrder.LITTLE_ENDIAN)
        bbuf.putInt(0)
        bbuf.putInt(stringTableMap(sym.name))
        sym.nameOrOffset = bbuf.array()
      } else {
        sym.nameOrOffset = sym.name.toCharArray().padTo(8, 0.toChar) map (_.toByte)
      }
    }
    
    val allReloc = Array.concat(relocations.map(_.apply): _*)
    val allSymbols = Array.concat(symbols.map(_.write): _*)
    
    allReloc ++ allSymbols ++ tableLength.array() ++ stringTable
  }
  
}

trait SymbolEntry {
  val bbuf = ByteBuffer.allocate(18)
  bbuf.order(ByteOrder.LITTLE_ENDIAN)
  def write: Array[Byte]
}

case class CoffSymbol (
    name: String,
    value: Int,
    sectionNumber: Short,
    symbolType: SymbolType,
    storageClass: StorageClass,
    auxiliarySymbols: Seq[SymbolEntry]) extends SymbolEntry {
  
  var nameOrOffset = Array[Byte]()
  
  override def toString = {
    "CoffSymbol(\"" + name + "\", " + value + ", " + sectionNumber + ')'
  }
  
  def write: Array[Byte] = {
    import scala.language.implicitConversions
    bbuf.rewind()
    // if the name is 8 char or longer, assume this is the position
    bbuf.put(nameOrOffset)
    bbuf.putInt(value)
    bbuf.putShort(sectionNumber)
    bbuf.putShort(symbolType.raw)
    bbuf.put(storageClass.value)
    bbuf.put(auxiliarySymbols.size.toByte)
    bbuf.array() ++ auxiliarySymbols.map(_.write).foldLeft(Array[Byte]())(_++_)
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
    selection: Byte) extends SymbolEntry {
  
  def write() = {
    bbuf.rewind()
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
    fileName: String) extends SymbolEntry {
  
  def write() = {
    bbuf.rewind()
    bbuf.put(fileName.padTo(18, 0.toChar).map(_.toByte).toArray)
    bbuf.array()
  } 
}