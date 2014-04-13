package com.scalaAsm.portableExe

import java.io.DataOutputStream
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap
import java.io.DataInputStream
import java.io.FileInputStream
import java.io.File
import java.nio.ByteBuffer
import java.nio.ByteOrder

private[portableExe] case class Extern(dllName: String, functionNames: Seq[String])

private[portableExe] case class ImageThunkDataRVA(offset: Int) extends AnyVal

private[portableExe] case class Imports(val imports: Seq[Extern], val offset: Int) {

  private def alignTo16(name: String) = if (name.size % 2 == 1) name + "\0" else name

  private trait Positionable extends ExeWriter {
    var position: Int = 0
  }

  private case class ImageImportByNameRVA(var offset: Int, dllName: String = "")

  private case class ImageThunkData(imagePointer: ImageImportByNameRVA, name: String) extends Positionable {
    def write(stream: DataOutputStream) {
      position = stream.size
      write(stream, imagePointer.offset)
    }
  }

  private case class ThunkArray(thunks: Seq[ImageThunkData], dllName: String) extends Positionable {

    def write(stream: DataOutputStream) {
      position = stream.size
      thunks.foreach(_.write(stream))
      stream.write(Array[Byte](0, 0, 0, 0))
    }
  }

  private trait ImportSymbol extends Positionable {
    val name: String
  }

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
      write(stream, importedDLLname.offset)
      write(stream, firstThunk.offset)
    }
  }

  private object ImageImportDescriptor {
    val size = 20;
  }

  def getExportSymbols = {
    val file = new File("C:/Windows/System32/msvcrt.dll");
    //val file = new File("C:/Users/Antares/Desktop/kernel32 - Copy.dll");
 
    val bFile: Array[Byte] = Array.fill(file.length().toInt)(0);
      
    //convert file into array of bytes
    val fileInputStream = new FileInputStream(file);
    fileInputStream.read(bFile);
    fileInputStream.close();
    
    val bbuf = ByteBuffer.wrap(bFile)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    
    val dosHeader = DosHeader.getDosHeader(bbuf)
    val peHeader = PeHeader.getPeHeader(bbuf)
    
    val dirs = DataDirectories.getDirectories(bbuf, peHeader.optionalHeader.additionalFields.numberOfRvaAndSizes)
    val sections = Sections.getSections(bbuf, peHeader.fileHeader.numberOfSections)
    println(dirs(0).virtualAddress - 4096)
    bbuf.position(dirs(0).virtualAddress - 4096)
    val export = ImageExportDirectory.getExports(bbuf, sections, dirs(0))
    export.functionNames.foreach(println)
  }
  
  def generateImports: CompiledImports = {
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
      ThunkArray(extern.functionNames.map(name => ImageThunkData(ImageImportByNameRVA(0), name)), extern.dllName)
    }

    val importAddressTable = imports.map(toAddressTable)
    val importNameTable = imports.map(toAddressTable)

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

    for (
      thunkArray <- importNameTable;
      thunk <- thunkArray.thunks
    ) {
      thunk.imagePointer.offset = offset + importByNames.find(importEntry => importEntry.name contains thunk.name).get.position
    }

    for (
      thunkArray <- importAddressTable;
      thunk <- thunkArray.thunks
    ) {
      thunk.imagePointer.offset = offset + importByNames.find(importEntry => importEntry.name contains thunk.name).get.position
    }

    val getCompleteFunctionMap = {
      (for (
        table <- importAddressTable;
        thunk <- table.thunks
      ) yield ((thunk.name.trim(), offset + thunk.position))).toMap
    }

    val byteOutput = new ByteArrayOutputStream()
    val stream = new DataOutputStream(byteOutput)

    // write the contents to the real stream

    importDescriptors.foreach(_.write(stream))
    importNameTable.foreach { _.write(stream) } // (INT)
    importAddressTable.foreach { _.write(stream) } // (IAT)
    importByNames.foreach(_.write(stream)) // Function names follow by dll name

    def getFunctionMap(externList: Seq[Extern]) = {
      val functionNames = getFunctionNames(externList)
      getCompleteFunctionMap.filter { case (fcnName, _) => functionNames contains fcnName }
    }

    CompiledImports(
      byteOutput.toByteArray(),
      importDescriptors.size * ImageImportDescriptor.size,
      importAddressTable.map(table => (table.thunks.size + 1) * 4).reduce(_ + _),
      getFunctionMap(imports)
    )
  }
}