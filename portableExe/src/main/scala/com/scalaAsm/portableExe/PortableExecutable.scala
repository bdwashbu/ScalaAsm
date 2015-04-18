package com.scalaAsm.portableExe

import sections._
import scala.collection.mutable.ArrayBuffer
import com.scalaAsm.coff.Section
import com.scalaAsm
import com.scalaAsm.x86.Instructions.instructionMap
import com.scalaAsm.x86._

case class PortableExecutable(dosHeader: DosHeader,
  peHeader: NtHeader,
  directories: DataDirectories,
  sections: Seq[Section]) {

  def align(array: Array[Byte], to: Int, filler: Byte = 0xCC.toByte) = {
    val currentSize = array.size
    val numPadding = (to - (currentSize % to)) % to
    array ++: Array.fill(numPadding)(filler)
  }

  def dissemble: Seq[String] = {
    val code = sections(0).contents
    println(sections(0).contents(0))
    for (i <- 0 to 3) {
      println("GO")
      val possible = instructionMap.instMap(code(i) & 0x000000FF).toSeq
      val result = possible.filter(inst => inst.opcode.isInstanceOf[OneOpcode] && inst.prefix.isEmpty).map(inst => inst.mnemonic).toSeq
      result.foreach(println)
    }
    null
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