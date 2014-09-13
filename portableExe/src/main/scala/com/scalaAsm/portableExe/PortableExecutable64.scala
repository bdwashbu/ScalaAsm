package com.scalaAsm.portableExe

import sections._
import scala.collection.mutable.ArrayBuffer

case class PortableExecutable64(dosHeader: DosHeader,
  peHeader: NtHeader,
  directories: DataDirectories,
  sections: Seq[Section]) extends PortableExecutable {

  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++: Array.fill(numPadding)(filler)
  }

  def get(): Array[Byte] = {
    
    val totalSize = peHeader.optionalHeader.additionalFields.sizeOfHeaders + sections.map(_.header.sizeOfRawData).sum
    
    val result = ArrayBuffer.fill(totalSize)(0.toByte)

    val headers = align(dosHeader(), 16, 0) ++:
      peHeader() ++:
      directories() ++:
      sections.map(_.header.write).reduce(_ ++ _)
    
    result.insertAll(0, headers)
    sections.foreach{section => result.insertAll(section.header.pointerToRawData, section.contents)}

    align(result.toArray, peHeader.optionalHeader.additionalFields.fileAlignment, 0x00)
  }

  override def toString = {
    dosHeader.toString + "\n" +
      peHeader.toString + "\n" +
      directories.toString + "\n" +
      sections.toString + "\n"
  }

}