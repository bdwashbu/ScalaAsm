package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.asm.CodeSection
import com.scalaAsm.asm.AsmProgram

object DosHeader {
  def getDosHeader(input: ByteBuffer): DosHeader = {

    input.order(ByteOrder.LITTLE_ENDIAN)
    val header = DosHeader(
      e_magic = List(input.get.toChar, input.get.toChar).mkString,
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
      e_res = (for (i <- 0 until 4) yield input.getShort()).toArray,
      e_oemid = input.getShort(),
      e_oeminfo = input.getShort(),
      e_res2 = (for (i <- 0 until 10) yield input.getShort()).toArray,
      e_lfanew = input.getInt()
    )
    input.position(header.e_lfanew) // skip dos stub
    header
  }
}

// Standard real-mode dos program that prints an error if it cannot be run

object DosStub {
  val dosWarning = "This program cannot be run in DOS mode.\r\r\n$"
  def putStub(bbuf: ByteBuffer) = {
    val dosStub = new CodeSection {
      builder += push(cs)
      builder += pop(ds)
      builder += mov(dx, imm16(0xE.toByte))
      builder += mov(ah, imm8(0x9))
      builder += int(imm8(0x21))
      builder += mov(ax, imm16(0x4C01))
      builder += int(imm8(0x21))
    }

    bbuf.put(dosStub.getRawBytes)
    bbuf.put(dosWarning.toCharArray() map (_.toByte))
  }
}

case class DosHeader(
  e_magic: String, // Magic number
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
  e_res: Array[Short], // Reserved words
  e_oemid: Short, // OEM identifier (for e_oeminfo)
  e_oeminfo: Short, // OEM information; e_oemid specific
  e_res2: Array[Short], // Reserved words
  e_lfanew: Int) { // File address of new exe header

  def apply() = {
    val bbuf = ByteBuffer.allocate(256);
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(e_magic.toCharArray() map (_.toByte))
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
    e_res.foreach(field => bbuf.putShort(field))
    bbuf.putShort(e_oemid)
    bbuf.putShort(e_oeminfo)
    e_res2.foreach(field => bbuf.putShort(field))
    bbuf.putInt(e_lfanew)

    DosStub.putStub(bbuf)
    bbuf.array().take(bbuf.position())
  }
}