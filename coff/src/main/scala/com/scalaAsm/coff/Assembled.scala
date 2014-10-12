package com.scalaAsm.coff

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object Assembled {
  def readCoff(filePath: String): Assembled = { 
    
    val file = new File(filePath);
 
    val bFile: Array[Byte] = Array.fill(file.length().toInt)(0);
      
    //convert file into array of bytes
    val fileInputStream = new FileInputStream(file);
    fileInputStream.read(bFile);
    fileInputStream.close();
    
    val bbuf = ByteBuffer.wrap(bFile)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    
    val coffHeader = CoffHeader.getCoffHeader(bbuf)
    // only image files have an optional header
    val sectionHeaders = 
      for (i <- 0 until coffHeader.numberOfSections)
         yield SectionHeader.getSectionHeader(bbuf)
    
    val sections = sectionHeaders map { header =>
      val sectionData = Array.fill[Byte](header.sizeOfRawData)(0)
      bbuf.get(sectionData, 0, header.sizeOfRawData)
      Section(header, sectionData)
    } 
    
    val relocations = sectionHeaders flatMap { header =>
      for (i <- 0 until header.relocations)
         yield RelocationEntry.getRelocationEntry(bbuf)
    }
    
    val symbolTable = new ListBuffer[SymbolEntry]()
    
    var symbolCount = 0
    while (symbolCount < coffHeader.numberOfSymbols) {
      val newSymbol = SymbolEntry.getSymbolEntry(bbuf)
      symbolCount += 1 + newSymbol.auxiliarySymbols.size
      println(symbolCount)
      symbolTable += newSymbol
    }
    
    val sizeOfStrings = bbuf.getInt()
    println("size: " + sizeOfStrings)
    val stringData = Array.fill[Byte](sizeOfStrings - 4)(0)
    bbuf.get(stringData, 0, sizeOfStrings - 4)
    
    
    var stringPos = 4
    val strings = new String(stringData).split('\u0000').map{ str =>
      val result = (stringPos, str)
      stringPos += str.length() + 1
      result
    }.toMap
    
    val symbols = symbolTable.map { entry =>
      if (entry.name(0) == '\u0000' && entry.name(1) == '\u0000' && entry.name(2) == '\u0000' && entry.name(3) == '\u0000') {
        val bbuf2 = ByteBuffer.allocate(4)
        bbuf2.order(ByteOrder.LITTLE_ENDIAN)
        bbuf2.put(entry.name(4).toByte)
        bbuf2.put(entry.name(5).toByte)
        bbuf2.put(entry.name(6).toByte)
        bbuf2.put(entry.name(7).toByte)
        bbuf2.rewind()
        val offset = bbuf2.getInt()
        entry.copy(name = strings(offset))
      } else {
        entry.copy(name = entry.name.trim)       
      }
    }
    
    println(coffHeader)
    sectionHeaders.foreach(println)
    sections.foreach(println)
    relocations.foreach(println)
    symbols.foreach(println)
    null
  }
}

abstract class Assembled(val iconPath: Option[String] = None) {
  self =>
  val rawData: Array[Byte]
  val rawCode: Array[Byte]
  val symbols: Seq[CoffSymbol]
  val relocations: ListBuffer[Relocation]

  def finalizeAssembly(addressOfData: Int, imports64: Map[String, Int], baseOffset: Int): ArrayBuffer[Byte]

  def addIcon(path: String): Assembled = {
    new Assembled(Option(path)) {
      val rawData = self.rawData
      val rawCode = self.rawCode
      val symbols = self.symbols
      val relocations = self.relocations

      def finalizeAssembly(addressOfData: Int, imports64: Map[String, Int], baseOffset: Int): ArrayBuffer[Byte] = {
        self.finalizeAssembly(addressOfData, imports64, baseOffset)
      }
    }
  }
}