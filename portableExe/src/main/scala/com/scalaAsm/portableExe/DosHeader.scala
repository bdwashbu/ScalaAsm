package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

object DosHeader {
    
  def getDosHeader(input: ByteBuffer): DosHeader = {

    input.order(ByteOrder.LITTLE_ENDIAN)
    val magic = List(input.get.toChar, input.get.toChar).mkString
    val header = DosHeader(
      e_cblp = input.getShort(),
      e_cp = input.getShort(),
      e_crlc = input.getShort(),
      e_cparhdr = input.getShort(),
      e_minalloc = input.getShort(),
      e_maxalloc = input.getShort(),
      e_ss = input.getShort(),
      e_sp = input.getShort(),
      e_csum = input.getShort(),
      e_ip = input.getShort(),
      e_cs = input.getShort(),
      e_lfarlc = input.getShort(),
      e_ovno = input.getShort(),
      e_res = (input.getShort(), input.getShort(), input.getShort(), input.getShort()),
      e_oemid = input.getShort(),
      e_oeminfo = input.getShort(),
      e_res2 = (input.getShort(), input.getShort(), input.getShort(), input.getShort(), input.getShort(),
                input.getShort(), input.getShort(), input.getShort(), input.getShort(), input.getShort())
    )
    header.magic = magic
    header.e_lfanew = input.getInt()
    input.position(header.e_lfanew) // skip dos stub
    header
  }
}

case class DosHeader(
  e_cblp: Short, // Bytes on last page of file
  e_cp: Short, // Pages in file
  e_crlc: Short, // Relocations
  e_cparhdr: Short, // Size of header in paragraphs
  e_minalloc: Short, // Minimum extra paragraphs needed
  e_maxalloc: Short, // Maximum extra paragraphs needed
  e_ss: Short, // Initial (relative) SS value
  e_sp: Short, // Initial SP value
  e_csum: Short, // Checksum
  e_ip: Short, // Initial IP value
  e_cs: Short, // Initial (relative) CS value
  e_lfarlc: Short, // File address of relocation table
  e_ovno: Short, // Overlay number
  e_res: Tuple4[Short, Short, Short, Short], // Reserved words
  e_oemid: Short, // OEM identifier (for e_oeminfo)
  e_oeminfo: Short, // OEM information; e_oemid specific
  e_res2: Tuple10[Short, Short, Short, Short, Short, Short, Short, Short, Short, Short], // Reserved words
  dosStub:  Array[Byte] = Array(),
  watermark: String = "") { // File address of new exe header
  
  var magic = "MZ" // Magic number
  var e_lfanew = 64 + watermark.length + 32 - watermark.length // relative offset to the NT Headers

  def apply() = {
    val bbuf = ByteBuffer.allocate(256);
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(magic.toCharArray() map (_.toByte))
    bbuf.putShort(e_cblp)
    bbuf.putShort(e_cp)
    bbuf.putShort(e_crlc)
    bbuf.putShort(e_cparhdr)
    bbuf.putShort(e_minalloc)
    bbuf.putShort(e_maxalloc)
    bbuf.putShort(e_ss)
    bbuf.putShort(e_sp)
    bbuf.putShort(e_csum)
    bbuf.putShort(e_ip)
    bbuf.putShort(e_cs)
    bbuf.putShort(e_lfarlc)
    bbuf.putShort(e_ovno)
    e_res.productIterator.foreach(field => bbuf.putShort(field.asInstanceOf[Short]))
    bbuf.putShort(e_oemid)
    bbuf.putShort(e_oeminfo)
    e_res2.productIterator.foreach(field => bbuf.putShort(field.asInstanceOf[Short]))
    bbuf.putInt(e_lfanew)
    bbuf.put(dosStub)
    bbuf.put(watermark.getBytes)
    bbuf.array().take(bbuf.position())
  }
}