package com.scalaAsm.portableExe

import sections._
import scala.collection.mutable.ArrayBuffer
import com.scalaAsm.coff.Section

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
    
    val totalSize = sections.last.header.pointerToRawData + sections.last.header.sizeOfRawData
    
    var result = ArrayBuffer.fill(totalSize)(0.toByte)
    
    val headers = align(dosHeader(), 32, 0) ++:
      peHeader() ++:
      directories() ++:
      sections.map(_.header.write).reduce(_ ++ _)
    
    result = result.patch(0, headers, headers.length)
    sections.foreach{section => result = result.patch(section.header.pointerToRawData, section.contents, section.contents.length)}

    result.toArray
  }

  override def toString = {
    dosHeader.toString + "\n" +
      peHeader.toString + "\n" +
      directories.toString + "\n" +
      sections.toString + "\n"
  }

}