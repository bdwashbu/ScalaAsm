package com.scalaAsm.portableExe

import java.io.DataOutputStream
import java.io.ByteArrayOutputStream
import scala.collection.mutable.ListBuffer

case class Extern(dllName: String, functionNames: Seq[String])

case class Imports(val externs: Seq[Extern], val nonExterns: Seq[Extern], val offset: Int) extends ExeWriter {

  case class ImageImportDescriptor (
    val originalFirstThunk: Int,
    val timeStamp: Int,
    val forwarderChain: Int,
    val importedDLLname: Int,
    val firstThunk: Int) extends ExeWriter {

    def write(stream: DataOutputStream) {
      write(stream, originalFirstThunk)
      write(stream, timeStamp)
      write(stream, forwarderChain)
      write(stream, importedDLLname)
      write(stream, firstThunk)
    }
  }

  def link: CompiledImports = {

    val imports = externs ++ nonExterns

    def padNames(name: String) = if (name.size % 2 == 0) Array[Byte](0x00, 0x00) else Array[Byte](0x00)

    def getDllNames(x: Seq[Extern]): List[String] = x.flatMap(x => List(x.dllName)).toList
    def getFunctionNames(x: Seq[Extern]): List[String] = x.flatMap(_.functionNames).toList

    case class BoundImport(val boundImportDescriptors: Seq[ImageImportDescriptor], val importAddressTable: Seq[Int])

    def getBoundImports(): BoundImport = {

      val initalLookupTableRVA: Map[String, Int] = Map.empty

      val nameRVAs: Map[String, Int] = {
        var position = 0
        val flattenedFcns: Seq[String] = imports.flatMap(x => x.functionNames ++ Seq(x.dllName))
        flattenedFcns.foldLeft(initalLookupTableRVA)((prev, name) => {
          val skipHint = if (getDllNames(imports).contains(name)) 0 else 2
          val result = prev ++ Map(name -> (position + skipHint))
          position += name.length + padNames(name).length + skipHint
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
      
      def getAddress(name: String) = offset + nameRVAs(name) + sizeOfAddrTable * 2 + (imports.size + 1) * 20 - 2
      
      def toAddressTable(extern: Extern) = extern.functionNames.map(getAddress) :+ 0

      val boundImports = getDllNames(imports).map { dllName =>

        val lookupAddr = offset + lookupTableRVAs(dllName) + (imports.size + 1) * 20 // assumes imports.size is the number of tables

        ImageImportDescriptor(
          originalFirstThunk = lookupAddr,
          timeStamp = 0x00000000,
          forwarderChain = 0x00000000,
          importedDLLname = offset + nameRVAs(dllName) + sizeOfAddrTable * 2 + (imports.size + 1) * 20,
          firstThunk = lookupAddr + sizeOfAddrTable)
      } :+ ImageImportDescriptor(0, 0, 0, 0, 0) // terminating descriptor

      val importAddressTable = imports.flatMap(toAddressTable)

      return BoundImport(boundImports, importAddressTable)
    }

    val BoundImport(boundImportDescriptors, importAddressTable) = getBoundImports()
    val importNameTable = importAddressTable // INT is the same as IAT

    val byteOutput = new ByteArrayOutputStream()
    val stream = new DataOutputStream(byteOutput)

    boundImportDescriptors.foreach(_.write(stream))
    importNameTable.foreach(x => write(stream, x)) // (INT)
    importAddressTable.foreach(x => write(stream, x)) // (IAT)

    for (importEntry <- imports) {
      for (functionName <- importEntry.functionNames)  {
        write(stream, 0.toShort)
        stream.write(functionName.toCharArray().map(_.toByte))
        stream.write(padNames(functionName))
      }
      stream.write(importEntry.dllName.toCharArray().map(_.toByte))
      stream.write(padNames(importEntry.dllName))
    }

    val getCompleteFunctionMap = {
      val flattenedFcns = imports.flatMap(imp => imp.functionNames ++ List(imp.dllName))
      val start = offset + (imports.size + 1) * 20 + flattenedFcns.size * 4
      
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