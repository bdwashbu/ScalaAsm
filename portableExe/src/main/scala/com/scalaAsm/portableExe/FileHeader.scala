package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

// aka COFF Header

sealed abstract class MachineType(val value: Short)
case object Intel386 extends MachineType(0x14c)
case object Intel860 extends MachineType(0x14d)
case object MIPS3000 extends MachineType(0x162)
case object MIPS4000 extends MachineType(0x166)
case object DecAlpha extends MachineType(0x183)
case object AMD64 extends MachineType(0x8664.toShort)

sealed abstract class Characteristic(val value: Short)

object FileHeader {
  
  val LargeAddresses: Short = 0x0020
  val DLL: Short = 0x2000
  val Executable: Short = 0x0002
  val is32Bit: Short = 0x0100
  val symbolsStripped: Short = 0x0008
  val linesStripped: Short = 0x0004
  
  def getFileHeader(input: ByteBuffer): FileHeader = {
    input.order(ByteOrder.LITTLE_ENDIAN)
    val header = FileHeader(
        machine = new MachineType(input.getShort()) {},
        numberOfSections = input.getShort(),
    	timeDateStamp = input.getInt(),
    	pointerToSymbolTable = input.getInt(),
    	numberOfSymbols = input.getInt(),
    	sizeOfOptionalHeader = input.getShort(),
    	characteristics = input.getShort()
    )
   
    header
  }
}

case class FileHeader(
    machine: MachineType,
    numberOfSections: Short,
    timeDateStamp: Int,
    pointerToSymbolTable: Int, // only .obj file
    numberOfSymbols: Int, // only .obj file
    sizeOfOptionalHeader: Short,
    characteristics: Short) {

    def apply() = {
      val bbuf = ByteBuffer.allocate(20);
      bbuf.order(ByteOrder.LITTLE_ENDIAN)
      bbuf.putShort(machine.value)
      bbuf.putShort(numberOfSections)
      bbuf.putInt(timeDateStamp)
      bbuf.putInt(pointerToSymbolTable)
      bbuf.putInt(numberOfSymbols)
      bbuf.putShort(sizeOfOptionalHeader)
      bbuf.putShort(characteristics)
      bbuf.array()
    }
  }