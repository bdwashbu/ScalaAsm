package com.scalaAsm.portableExe

import sections._
import scala.collection.mutable.ArrayBuffer

case class PortableExecutable64(dosHeader: DosHeader,
  peHeader: NtHeader,
  directories: DataDirectories,
  sections: Seq[SectionHeader],
  code: Array[Byte],
  rawData: Array[Byte],
  compiledImports: CompiledImports,
  resources: Array[Byte]) extends PortableExecutable {

  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++: Array.fill(numPadding)(filler)
  }

  def get(): Array[Byte] = {
    
    val totalSize = peHeader.optionalHeader.additionalFields.sizeOfHeaders + sections.map(_.sizeOfRawData).sum
    
    val result = ArrayBuffer.fill(totalSize)(0.toByte)

    val headers = align(dosHeader(), 16, 0) ++:
      peHeader() ++:
      directories() ++:
      sections.map(_.write).reduce(_ ++ _)
    
    result.insertAll(0, headers)
    result.insertAll(0x200, code)
    result.insertAll(0x400, rawData)
    result.insertAll(0x600, compiledImports.rawData)

    align(result.toArray, peHeader.optionalHeader.additionalFields.fileAlignment, 0x00)
  }

  override def toString = {
    dosHeader.toString + "\n" +
      peHeader.toString + "\n" +
      directories.toString + "\n" +
      sections.toString + "\n"
  }

}