package com.scalaAsm.portableExe
package sections

import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer
import java.nio.ByteOrder

private[portableExe] case class CompiledSections(sectionheaders: SectionHeader*) {
  val sectionHeaders = sectionheaders map(_.write) reduce(_ ++ _)
}

object SectionHeader {
  def getSectionHeader(input: ByteBuffer): SectionHeader = {
     val name = Array.fill(8)(0.toByte)
     input.get(name)
     SectionHeader(
         name = name map(_.toChar) mkString,
         virtualSize = input.getInt(),
         virtualAddress   = input.getInt(),
         sizeOfRawData    = input.getInt(),
         pointerToRawData = input.getInt(),
         relocPtr    = input.getInt(),
         linenumPtr  = input.getInt(),
         relocations = input.getShort(),
         lineNumbers = input.getShort(),
         characteristics = input.getInt()
     )
  }
}

case class SectionHeader(
  name: String,
  virtualSize: Int,
  virtualAddress: Int,
  sizeOfRawData: Int,
  pointerToRawData: Int,
  relocPtr: Int,
  linenumPtr: Int,
  relocations: Short,
  lineNumbers: Short,
  characteristics: Int) {

  def write: Array[Byte] = {
    val bbuf = ByteBuffer.allocate(256);
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(name.padTo(8, 0.toChar) map(_.toByte) toArray)
    bbuf.putInt(virtualSize)
    bbuf.putInt(virtualAddress)
    bbuf.putInt(sizeOfRawData)
    bbuf.putInt(pointerToRawData)
    bbuf.putInt(relocPtr)
    bbuf.putInt(linenumPtr)
    bbuf.putShort(relocations)
    bbuf.putShort(lineNumbers)
    bbuf.putInt(characteristics)
    bbuf.array().take(bbuf.position())
  }
}

object Characteristic extends Enumeration {
  type characteristic = Value
  val CODE        = Value(0x00000020)
  val INITIALIZED = Value(0x00000040)
  val UNINIT_DATA = Value(0x00000080)
  val NOT_CACHE   = Value(0x04000000)
  val NOT_PAGE    = Value(0x08000000)
  val SHARED      = Value(0x10000000)
  val EXECUTE     = Value(0x20000000)
  val READ        = Value(0x40000000)
  val WRITE       = Value(0x80000000)
}

object Sections {
  def getSections(input: ByteBuffer, numSections: Int): Seq[SectionHeader] = {
    val sectionHeaders = new ListBuffer[SectionHeader]()

    for (i <- 0 until numSections) {
      val sectionHeader = SectionHeader.getSectionHeader(input)
      sectionHeaders += sectionHeader
    }
    sectionHeaders.toSeq
  }
}