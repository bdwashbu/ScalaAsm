package com.scalaAsm.portableExe
package sections

import java.io.DataOutputStream
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap
import java.io.DataInputStream
import java.io.FileInputStream
import java.io.File
import java.nio.ByteBuffer
import java.nio.ByteOrder

case class Extern(dllName: String, functionNames: Seq[String])

private[portableExe] case class ImageThunkDataRVA(offset: Int) extends AnyVal

trait Positionable extends ExeWriter {
    var position: Int = 0
  }

  case class ImageImportByNameRVA(var offset: Long, dllName: String = "")

  case class ImageThunkData(imagePointer: ImageImportByNameRVA, name: String, is64Bit: Boolean) extends Positionable {
    def write(stream: DataOutputStream) {
      position = stream.size
      if (is64Bit)
    	  write(stream, imagePointer.offset)
      else
        write(stream, imagePointer.offset.toInt)
    }
  }
  
  object ImageThunkData {
    def size(is64Bit: Boolean) = if (is64Bit) 8 else 4
  }

  case class ThunkArray(thunks: Seq[ImageThunkData], dllName: String, is64Bit: Boolean) extends Positionable {

    def write(stream: DataOutputStream) {
      position = stream.size
      thunks.foreach(_.write(stream))
      if (is64Bit)
        stream.write(Array[Byte](0, 0, 0, 0, 0, 0, 0, 0))
      else
        stream.write(Array[Byte](0, 0, 0, 0))
    }
  }

trait ImportSymbol extends Positionable {
    val name: String
  }
  
case class Imports(val imports: Seq[Extern], val offset: Int) {

  private def alignTo16(name: String) = if (name.size % 2 == 1) name + "\0" else name

  private case class ImageImportByName(hint: Short = 0, name: String) extends ImportSymbol {

    def write(stream: DataOutputStream) {
      position = stream.size
      write(stream, hint)
      stream.write(alignTo16(name).toCharArray().map(_.toByte))
    }
  }

  private case class DLLName(name: String) extends ImportSymbol {

    def write(stream: DataOutputStream) {
      position = stream.size
      stream.write(name.toCharArray().map(_.toByte))
    }
  }

  private case class ImageImportDescriptor(
    var originalFirstThunk: ImageThunkDataRVA,
    val timeStamp: Int,
    val forwarderChain: Int, // -1 if no forwarders
    var importedDLLname: ImageImportByNameRVA,
    var firstThunk: ImageThunkDataRVA)
    extends Positionable {

    def write(stream: DataOutputStream) {
      position = stream.size
      write(stream, originalFirstThunk.offset)
      write(stream, timeStamp)
      write(stream, forwarderChain)
      write(stream, importedDLLname.offset.toInt)
      write(stream, firstThunk.offset)
    }
  }

  private object ImageImportDescriptor {
    val size = 20;
  }
  
  def generateImports(is64Bit: Boolean): CompiledImports = {
    val numImportsPlusNull = imports.size + 1

    def getDllNames(externs: Seq[Extern]): List[String] = externs.map(_.dllName).toList
    def getFunctionNames(externs: Seq[Extern]): List[String] = externs.flatMap(_.functionNames).toList

    val initalLookupTableRVA: TreeMap[String, Int] = TreeMap.empty

    val terminator = ImageImportDescriptor(ImageThunkDataRVA(0), 0, 0, ImageImportByNameRVA(0, ""), ImageThunkDataRVA(0))

    val importDescriptors = getDllNames(imports).map { dllName =>

      ImageImportDescriptor(
        originalFirstThunk = ImageThunkDataRVA(0),
        timeStamp = 0x00000000,
        forwarderChain = 0x00000000,
        importedDLLname = ImageImportByNameRVA(0, dllName),
        firstThunk = ImageThunkDataRVA(0))

    } :+ terminator

    def toAddressTable(extern: Extern): ThunkArray = {
      ThunkArray(extern.functionNames.map(name => ImageThunkData(ImageImportByNameRVA(0), name, is64Bit)), extern.dllName, is64Bit)
    }

    val importAddressTable = imports map toAddressTable
    val importNameTable = importAddressTable

    val firstByteOutput = new ByteArrayOutputStream()
    val firstStream = new DataOutputStream(firstByteOutput)

    val importByNames: Seq[ImportSymbol] = imports.flatMap { importEntry =>
      importEntry.functionNames.map(name => ImageImportByName(0, name + "\0")) :+ DLLName(alignTo16(importEntry.dllName + "\0"))
    }

    // first, just place everything into a temp stream. we dont care about the contents, we just want position values.

    importDescriptors.foreach(_.write(firstStream))
    importNameTable.foreach {_.write(firstStream) } // (INT)
    importAddressTable.foreach {_.write(firstStream) } // (IAT)
    importByNames.foreach(_.write(firstStream))

    // use the position values to fill in contents

    for (descriptor <- importDescriptors.take(importDescriptors.size - 1)) {

      val firstThunkRVA = offset + importAddressTable.find(x => x.dllName.trim == descriptor.importedDLLname.dllName).get.position
      val originalFirstThunkRVA = offset + importNameTable.find(x => x.dllName.trim == descriptor.importedDLLname.dllName).get.position

      descriptor.importedDLLname = ImageImportByNameRVA(offset + importByNames.find(x => x.name.trim == descriptor.importedDLLname.dllName).get.position)
      descriptor.firstThunk = ImageThunkDataRVA(firstThunkRVA) //point to Import Address Table (IAT)
      descriptor.originalFirstThunk = ImageThunkDataRVA(originalFirstThunkRVA) //point to Import Name Table (INT)
    }

    def setThunkOffsets(table: Seq[com.scalaAsm.portableExe.sections.ThunkArray]) {
      for (
        thunkArray <- table;
        thunk <- thunkArray.thunks
      ) {
        thunk.imagePointer.offset = offset + importByNames.find(importEntry => importEntry.name contains thunk.name).get.position
      }
    }
    
    setThunkOffsets(importNameTable)
    setThunkOffsets(importAddressTable)

    val byteOutput = new ByteArrayOutputStream()
    val stream = new DataOutputStream(byteOutput)

    val importDescriptorOffset = importDescriptors.size * ImageImportDescriptor.size
    
    // write the contents to the real stream

    if (is64Bit) {
      
       def swap(x: Int): Array[Byte] = {
        val buffer = ByteBuffer.allocate(4)
        buffer.order(ByteOrder.LITTLE_ENDIAN)
        buffer.putInt(x)
        buffer.array()
      }
      
      val importOffset = importDescriptorOffset + (importNameTable.size + 1) * ImageThunkData.size(is64Bit) * 2
      val numThunks = importAddressTable.map(imports => imports.thunks.size).sum
       
      // add jmps for the imports
      
      var offset = 0
      importAddressTable.foreach { imp =>
        imp.thunks.foreach { _ =>
          stream.write(Array(0xFF.toByte, 0x25.toByte))
          stream.write(swap(importOffset + (numThunks - 1) * 6 + offset))
          offset += (ImageThunkData.size(is64Bit) - 6) // 6 is the size of the jmp instruction
        }
        offset += 8 // jump past terminator
      }
    }
    importDescriptors.foreach(_.write(stream))
    importNameTable.foreach { _.write(stream) } // (INT)
    importAddressTable.foreach { _.write(stream) } // (IAT)
    importByNames.foreach(_.write(stream)) // Function names follow by dll name

    def getFunctionMap(externList: Seq[Extern]) = {
      val functionNames = getFunctionNames(externList)
      
      (for (
        table <- importAddressTable;
        thunk <- table.thunks if functionNames contains thunk.name.trim()
      ) yield ((thunk.name.trim(), offset + thunk.position))).toMap
    }

    CompiledImports(
      byteOutput.toByteArray(),
      importDescriptorOffset,
      importAddressTable.map(table => (table.thunks.size + 1) * (if (is64Bit) 8 else 4)).reduce(_ + _),
      getFunctionMap(imports),
      importByNames
    )
  }
}