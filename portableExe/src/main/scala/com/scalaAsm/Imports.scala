package com.scalaAsm.portableExe

import java.io.DataOutputStream
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ListBuffer

case class Extern(dllName: String, functionNames: Seq[String])

case class Imports(val externs: Seq[Extern], val nonExterns: Seq[Extern], val offset: Int) extends ExeWriter {

  type Address = Int
  
  case class ImageThunkDataRVA(offset: Int)
  case class ImportByNameRVA(offset: Int)
  
  case class ImageImportByName(hint: Short = 0, name: String) extends ExeWriter {
    def write(stream: DataOutputStream) {
     def padString(name: String) = if (name.size % 2 != 0) Array[Byte](0x00) else Array[Byte]()
    	  
       write(stream, 0.toShort)
       stream.write(name.toCharArray().map(_.toByte))
       stream.write(padString(name))
    }
  }
  
  case class ImageImportDescriptor (
    var originalFirstThunk: ImageThunkDataRVA,
    val timeStamp: Int,
    val forwarderChain: Int, // -1 if no forwarders
    var importedDLLname: Int,
    var firstThunk: ImageThunkDataRVA,
    val dllName: String)
      extends ExeWriter {

    def write(stream: DataOutputStream) {
      write(stream, originalFirstThunk.offset)
      write(stream, timeStamp)
      write(stream, forwarderChain)
      write(stream, importedDLLname)
      write(stream, firstThunk.offset)
    }
  }
  
  object ImageImportDescriptor 
  {
    val size = 20;
  }

  def link: CompiledImports = {

    val imports = externs ++ nonExterns
    val numImportsPlusNull = imports.size + 1

    def padString(name: String) = if (name.size % 2 != 0) Array[Byte](0x00) else Array[Byte]()

    def getDllNames(x: Seq[Extern]): List[String] = x.flatMap(x => List(x.dllName)).toList
    def getFunctionNames(x: Seq[Extern]): List[String] = x.flatMap(_.functionNames).toList

    case class BoundImport(val boundImportDescriptors: Seq[ImageImportDescriptor], val importAddressList: Seq[Int])

    def getBoundImports(): BoundImport = {

      val initalLookupTableRVA: Map[String, Int] = Map.empty

      type ImageImportAddress = Int
      
      val nameRVAs: Map[String, ImageImportAddress] = {
        var position = 0
        val flattenedFcns: Seq[String] = imports.flatMap(x => x.functionNames.map(_ + "\0") ++ Seq(x.dllName + "\0"))
        
        flattenedFcns.foldLeft(initalLookupTableRVA)((prev, name) => {
          
          val noNull = name.reverse.tail.reverse
          val skipHint = if (getDllNames(imports).contains(noNull)) 0 else 2
          
          val result = prev ++ Map(noNull -> (position + skipHint))
          position += name.length + padString(name).length + skipHint
          result
        })
      }

      val lookupTableRVAs: Map[String, Int] = {
        var position = 0
        imports.foldLeft(initalLookupTableRVA)((offset, dll) => {

          val result = offset ++ Map(dll.dllName -> position)
          position += dll.functionNames.size * 4 + 4 //plus four for the 4 bytes padding
          result
        })
      }

      val importFunctionNames = getFunctionNames(imports)
      val sizeOfAddrTable = importFunctionNames.size * 4 + getDllNames(imports).size * 4

      val importDescriptors = getDllNames(imports).map { dllName =>

        ImageImportDescriptor(
          originalFirstThunk = null,
          timeStamp       = 0x00000000,
          forwarderChain  = 0x00000000,
          importedDLLname = 0,
          firstThunk = null,
          dllName = dllName)
        
      } :+ ImageImportDescriptor(ImageThunkDataRVA(0), 0, 0, 0, ImageThunkDataRVA(0), "") // terminating descriptor
      
      val descriptorSize = importDescriptors.size * ImageImportDescriptor.size
      
      for (descriptor <- importDescriptors.take(importDescriptors.size-1)) {
        val lookupAddr = offset + lookupTableRVAs(descriptor.dllName) + descriptorSize // assumes imports.size is the number of tables
        
        descriptor.importedDLLname = offset + nameRVAs(descriptor.dllName) + sizeOfAddrTable * 2 + descriptorSize
        descriptor.firstThunk = ImageThunkDataRVA(lookupAddr + sizeOfAddrTable)
        descriptor.originalFirstThunk = ImageThunkDataRVA(lookupAddr)
      }

      def getRVAAddress(name: String): Address = offset + nameRVAs(name) + sizeOfAddrTable * 2 + descriptorSize - 2
      def toAddressTable(extern: Extern) = extern.functionNames.map(getRVAAddress) :+ 0
      val importAddressTable = imports.flatMap(toAddressTable)

      return BoundImport(importDescriptors, importAddressTable)
    }

    val BoundImport(boundImportDescriptors, importAddressTable) = getBoundImports()
    val importNameTable = importAddressTable // INT is the same as IAT

    val byteOutput = new ByteArrayOutputStream()
    val stream = new DataOutputStream(byteOutput)

    boundImportDescriptors.foreach(_.write(stream))
    importNameTable.foreach(x => write(stream, x)) // (INT)
    importAddressTable.foreach(x => write(stream, x)) // (IAT)

    for (importEntry <- imports) {
      
      val importsByName = importEntry.functionNames.map(name => ImageImportByName(0, name + "\0"))
      
      importsByName.foreach(importName => importName.write(stream))
      val dllName = importEntry.dllName + "\0"
      stream.write(dllName.toCharArray().map(_.toByte))
      stream.write(padString(dllName))
    }

    val getCompleteFunctionMap = {
      val flattenedFcns = imports.flatMap(imp => imp.functionNames ++ List(imp.dllName))
      val start = offset + numImportsPlusNull * ImageImportDescriptor.size + flattenedFcns.size * 4
      
      val mapEntries = new ListBuffer[(String, Int)]()
      var index = 0
      for (importEntry <- imports) {
        for (functionName <- importEntry.functionNames)  {
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
                    boundImportDescriptors.size * 20,
                    importAddressTable.size * 4,
                    getFunctionMap(nonExterns),
                    getFunctionMap(externs))
  }
}