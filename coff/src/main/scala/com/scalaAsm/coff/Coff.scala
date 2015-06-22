package com.scalaAsm.coff

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.SortedMap

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
        yield Relocation.getRelocation(bbuf)
    }

    val symbolTable = new ListBuffer[(Int, CoffSymbol)]()

    // read the symbols, including auxiliary entries
    var symbolCount = 0
    while (symbolCount < coffHeader.numberOfSymbols) {
      val newSymbol = CoffSymbol.getSymbolEntry(bbuf)
      symbolTable += ((symbolCount, newSymbol))
      symbolCount += 1 + newSymbol.auxiliarySymbols.size
    }

    // read string table
    val sizeOfStrings = bbuf.getInt()
    val stringData = Array.fill[Byte](sizeOfStrings - 4)(0)
    bbuf.get(stringData, 0, sizeOfStrings - 4)

    // Create a map from the strings position to the string
    var stringPos = 4
    val stringPositionMap = new String(stringData).split('\u0000').map { str =>
      val result = (stringPos, str)
      stringPos += str.length() + 1
      result
    }.toMap

    // resolve the 'name' field and create a map from a symbols index to the CoffSymbol
    val symbolMap: SortedMap[Int, CoffSymbol] = SortedMap(symbolTable.map {
      case (index, sym) =>
        val copy = if (sym.isStringReference) {
          sym.copy(name = stringPositionMap(sym.getStringOffset))
        } else {
          sym.copy(name = sym.name.trim)
        }
        (index, (CoffSymbol(copy.name, copy.value, copy.sectionNumber, copy.symbolType, copy.storageClass, copy.auxiliarySymbols)))
    }: _*)

    // resolve the relocations based on the relocation's index
    val resolvedRelocations = relocations.toSeq map { reloc =>
      Relocation(
        reloc.referenceAddress,
        reloc.symbolIndex,
        reloc.relocationType)
    }

    Coff(sections, resolvedRelocations, symbolMap.values.toSeq, None)
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
    
    val numSymbolsAndAux = symbols.map{sym => 1 + sym.auxiliarySymbols.size}.sum

    val sectionHeaders = sections.map(_.header.write).reduce(_ ++ _)

    val allSections = sections.map(_.contents).reduce(_ ++ _)
    
    val sectionData = sectionHeaders ++ allSections
    
    val header = CoffHeader(
      machine = if (is64Bit) 0x8664.toShort else 0x14C,
      numberOfSections = sections.size.toShort,
      timeDateStamp = 0x535BF29F,
      pointerToSymbolTable = 20 + sectionData.length + relocations.size*10,
      numberOfSymbols = numSymbolsAndAux,
      sizeOfOptionalHeader = 0,
      characteristics = IMAGE_FILE_32BIT_MACHINE.value)

    outputStream.write(header() ++ sectionData ++ CoffSymbol.writeRelocationsAndSymbols(relocations, symbols))
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