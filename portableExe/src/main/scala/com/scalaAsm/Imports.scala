package com.scalaAsm.portableExe

import java.io.DataOutputStream
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.TreeMap

case class Extern(dllName: String, functionNames: Seq[String])

case class ImageThunkDataRVA(offset: Int) extends AnyVal

case class Imports(val externs: Seq[Extern], val nonExterns: Seq[Extern], val offset: Int) {

  def alignTo16(name: String) = if (name.size % 2 == 1) name + "\0" else name

  trait Positionable extends ExeWriter {
    var position: Int = 0
  }

  case class ImageImportByNameRVA(var offset: Int, dllName: String = "")

  case class ImageThunkData(imagePointer: ImageImportByNameRVA, name: String) extends ExeWriter {
    def write(stream: DataOutputStream) {
      write(stream, imagePointer.offset)
    }
  }

  case class ThunkArray(thunks: Seq[ImageThunkData], dllName: String) extends Positionable {

    def write(stream: DataOutputStream) {
      position = stream.size
      for (thunk <- thunks) {
        thunk.write(stream)
      }
      stream.write(Array[Byte](0, 0, 0, 0))
    }
  }

  trait ImportSymbol extends Positionable {
    val name: String
  }

  case class ImageImportByName(hint: Short = 0, name: String) extends ImportSymbol {

    def write(stream: DataOutputStream) {
      position = stream.size
      write(stream, hint)
      stream.write(alignTo16(name).toCharArray().map(_.toByte))
    }
  }

  case class DLLName(val name: String) extends ImportSymbol {

    def write(stream: DataOutputStream) {
      position = stream.size
      stream.write(name.toCharArray().map(_.toByte))
    }
  }

  case class ImageImportDescriptor(
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

  object ImageImportDescriptor {
    val size = 20;
  }

  def link: CompiledImports = {

    val imports = externs ++ nonExterns
    val numImportsPlusNull = imports.size + 1

    def getDllNames(x: Seq[Extern]): List[String] = x.flatMap(x => List(x.dllName)).toList
    def getFunctionNames(x: Seq[Extern]): List[String] = x.flatMap(_.functionNames).toList

    case class BoundImport(val boundImportDescriptors: Seq[ImageImportDescriptor], val importAddressList: Seq[ThunkArray], val importNameList: Seq[ThunkArray])

    val initalLookupTableRVA: TreeMap[String, Int] = TreeMap.empty

      val nullImportDescriptor = ImageImportDescriptor(ImageThunkDataRVA(0), 0, 0, ImageImportByNameRVA(0, ""), ImageThunkDataRVA(0))

      val importDescriptors = getDllNames(imports).map { dllName =>

        ImageImportDescriptor(
          originalFirstThunk = ImageThunkDataRVA(0),
          timeStamp = 0x00000000,
          forwarderChain = 0x00000000,
          importedDLLname = ImageImportByNameRVA(0, dllName),
          firstThunk = ImageThunkDataRVA(0))

      } :+ nullImportDescriptor

      def toAddressTable(extern: Extern): ThunkArray = {
        ThunkArray(extern.functionNames.map(x => ImageThunkData(ImageImportByNameRVA(0), x)), extern.dllName)
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
    importNameTable.foreach { x => x.write(firstStream) } // (INT)
    importAddressTable.foreach { x => x.write(firstStream) } // (IAT)
    importByNames.foreach(_.write(firstStream))
    
    // use the position values to fill in contents
    
    for (descriptor <- importDescriptors.take(importDescriptors.size - 1)) {
      
      val firstThunkRVA = offset + importAddressTable.find(x => x.dllName contains descriptor.importedDLLname.dllName).get.position 
      val originalFirstThunkRVA = offset + importNameTable.find(x => x.dllName contains descriptor.importedDLLname.dllName).get.position 
      
      descriptor.importedDLLname = ImageImportByNameRVA(offset + importByNames.find(x => x.name contains descriptor.importedDLLname.dllName).get.position)
      descriptor.firstThunk = ImageThunkDataRVA(firstThunkRVA) //point to Import Address Table (IAT)
      descriptor.originalFirstThunk = ImageThunkDataRVA(originalFirstThunkRVA) //point to Import Name Table (INT)
    }
    
    for (thunkArray <- importNameTable;
         thunk <- thunkArray.thunks) {
      thunk.imagePointer.offset = offset + importByNames.find(x => x.name contains thunk.name).get.position
    }
    
    for (thunkArray <- importAddressTable;
         thunk <- thunkArray.thunks) {
      thunk.imagePointer.offset = offset + importByNames.find(x => x.name contains thunk.name).get.position
    }

    val byteOutput = new ByteArrayOutputStream()
    val stream = new DataOutputStream(byteOutput)

    // write the contents to the real stream
    
    importDescriptors.foreach(_.write(stream))
    importNameTable.foreach { x => x.write(stream) } // (INT)
    importAddressTable.foreach { x => x.write(stream) } // (IAT)
    importByNames.foreach(_.write(stream)) // Function names follow by dll name

    val getCompleteFunctionMap = {
      val flattenedFcns = imports.flatMap(imp => imp.functionNames ++ List(imp.dllName))
      val start = offset + numImportsPlusNull * ImageImportDescriptor.size + flattenedFcns.size * 4

      val mapEntries = new ListBuffer[(String, Int)]()
      var index = 0
      for (importEntry <- imports) {
        for (functionName <- importEntry.functionNames) {
          mapEntries += ((functionName, start + index * 4))
          index = index + 1
        }
        index = index + 1
      }

      mapEntries.toMap
    }

    def getFunctionMap(externList: Seq[Extern]) = {
      val functionNames = getFunctionNames(externList)
      getCompleteFunctionMap.filter(x => functionNames.contains(x._1))
    }

    CompiledImports(byteOutput.toByteArray(),
      importDescriptors.size * ImageImportDescriptor.size,
      importAddressTable.map(x => (x.thunks.size + 1) * 4).reduce(_ + _),
      getFunctionMap(nonExterns),
      getFunctionMap(externs))
  }
}