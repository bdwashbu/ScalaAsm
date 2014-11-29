package com.scalaAsm.coff

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object Coff {
  def readCoff(filePath: String): Coff = { 
    
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
      val newSymbol = StandardSymbolEntry.getSymbolEntry(bbuf)
      symbolCount += 1 + newSymbol.auxiliarySymbols.size
      println(symbolCount)
      symbolTable ++= (newSymbol +: newSymbol.auxiliarySymbols)
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
    
    val symbols: Map[Int, CoffSymbol] = symbolTable.zipWithIndex.flatMap { entry => entry match {
      case (sym @ StandardSymbolEntry(symName,_,_,_,_,_), index) =>
        if (symName(0) == '\u0000' && symName(1) == '\u0000' && symName(2) == '\u0000' && symName(3) == '\u0000') {
          val bbuf2 = ByteBuffer.allocate(4)
          bbuf2.order(ByteOrder.LITTLE_ENDIAN)
          bbuf2.put(symName(4).toByte)
          bbuf2.put(symName(5).toByte)
          bbuf2.put(symName(6).toByte)
          bbuf2.put(symName(7).toByte)
          bbuf2.rewind()
          val offset = bbuf2.getInt()
          val copy = sym.copy(name = strings(offset))
          Some(index, CoffSymbol(copy.name, copy.value, copy.sectionNumber, copy.symbolType, copy.storageClass))
        } else {
          val copy = sym.copy(name = sym.name.trim)
          Some(index, CoffSymbol(copy.name, copy.value, copy.sectionNumber, copy.symbolType, copy.storageClass))
        }
      case _ => None
      }
    }.toMap
    
    // resolve the strings
    val resolvedRelocations = relocations.toSeq map { reloc =>
      Relocation (
        reloc.referenceAddress,
        symbols(reloc.symbolIndex),
        reloc.reloationType) }
    
    Coff(sections, resolvedRelocations, symbols.values.toSeq, None)
  }
}

case class Coff(sections: Seq[Section], relocations: Seq[Relocation], symbols: Seq[CoffSymbol], val iconPath: Option[String] = None) {
  def addIcon(path: String): Coff = {
    Coff(sections, relocations, symbols, Option(path))
  }
  
  def write(fileName: String) = {
    import java.io._
    val is64Bit = false
    
    val outputStream = new DataOutputStream(new FileOutputStream(fileName));
    
    val header = CoffHeader(
      machine = if (is64Bit) 0x8664.toShort else 0x14C,
      numberOfSections = sections.size.toShort,
      timeDateStamp = 0x535BF29F,
      pointerToSymbolTable = 0, // no importance
      numberOfSymbols = symbols.size, // no importance
      sizeOfOptionalHeader = 0,
      characteristics = if (is64Bit) 47 else 271)
      
    val sectionHeaders = sections map { section =>
      section.header
    }
    
    val allSections = sections.map(_.contents).reduce(_++_)
    val allReloc = relocations.map(_.apply()).reduce(_++_)
    
    outputStream.write(header() ++ sectionHeaders.map(_.write).reduce(_ ++ _) ++ allSections ++ allReloc)
    outputStream.close()
  }
  
  override def toString: String = {
    var result = "Coff(\n"
    for (section <- sections) {
      result += "   " + section.toString + "\n"
    }
    result += "   -----------------------\n"
    for (symbol <- symbols) {
      result += "   " + symbol.toString + "\n"
    }
    result += "   -----------------------\n"
    for (relocation <- relocations) {
      result += "   " + relocation.toString + "\n"
    }
    result += ")"
    result
  }
}