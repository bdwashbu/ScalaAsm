package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.asm.CodeSection
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens.Procedure

object DosHeader {
  def getDosHeader(input: ByteBuffer): DosHeader = {
    val header = new DosHeader
    input.order(ByteOrder.LITTLE_ENDIAN)
    header.e_magic = List(input.get.toChar, input.get.toChar).mkString
    header.e_cblp = input.getShort()
    header.e_cp = input.getShort()
    header.e_cparhdr = input.getShort()
    header.e_minalloc = input.getShort()
    header.e_maxalloc = input.getShort()
    header.e_ss = input.getShort()
    header.e_sp = input.getShort()
    header.e_csum = input.getShort()
    header.e_ip = input.getShort()
    header.e_cs = input.getShort()
    header.e_lfarlc = input.getShort()
    header.e_ovno = input.getShort()
    header.e_res = (for (i <- 0 until 4) yield input.getShort()).toArray
    header.e_oemid = input.getShort()
    header.e_oeminfo = input.getShort()
    header.e_res2 = (for (i <- 0 until 10) yield input.getShort()).toArray
    header.e_lfanew = input.getInt()
    header
  }
}

class DosHeader {
  var e_magic = "MZ" // Magic number
  var e_cblp: Short = 144 // Bytes on last page of file
  var e_cp: Short = 3 // Pages in file
  var e_crlc: Short = 0 // Relocations
  var e_cparhdr: Short = 4 // Size of header in paragraphs
  var e_minalloc: Short = 0 // Minimum extra paragraphs needed
  var e_maxalloc: Short = 65535.toShort // Maximum extra paragraphs needed
  var e_ss: Short = 0 // Initial (relative) SS value
  var e_sp: Short = 184 // Initial SP value
  var e_csum: Short = 0 // Checksum
  var e_ip: Short = 0 // Initial IP value
  var e_cs: Short = 0 // Initial (relative) CS value
  var e_lfarlc: Short = 64 // File address of relocation table
  var e_ovno: Short = 0 // Overlay number
  var e_res = Array.fill(4)(0.toShort) // Reserved words
  var e_oemid: Short = 0 // OEM identifier (for e_oeminfo)
  var e_oeminfo: Short = 0 // OEM information; e_oemid specific
  var e_res2 = Array.fill(10)(0.toShort) // Reserved words
  var e_lfanew: Int = 128 // File address of new exe header

  var dosWarning = "This program cannot be run in DOS mode.\r\r\n$"

  def apply() = {
    val bbuf = ByteBuffer.allocate(256);
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    bbuf.put(e_magic.toCharArray().map(_.toByte))
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
    bbuf.put(dosWarning.toCharArray().map(_.toByte))
    bbuf.array().take(bbuf.position())
  }
}