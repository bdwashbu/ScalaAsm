package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

// aka COFF Header

object FileHeader {
  def getFileHeader(input: ByteBuffer): FileHeader = {
    input.order(ByteOrder.LITTLE_ENDIAN)
    val header = FileHeader(
        machine = input.getShort(),
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
    machine: Short,
    numberOfSections: Short,
    timeDateStamp: Int,
    pointerToSymbolTable: Int, // only .obj file
    numberOfSymbols: Int, // only .obj file
    sizeOfOptionalHeader: Short,
    characteristics: Short) {

    def apply() = {
      val bbuf = ByteBuffer.allocate(20);
      bbuf.order(ByteOrder.LITTLE_ENDIAN)
      bbuf.putShort(machine)
      bbuf.putShort(numberOfSections)
      bbuf.putInt(timeDateStamp)
      bbuf.putInt(pointerToSymbolTable)
      bbuf.putInt(numberOfSymbols)
      bbuf.putShort(sizeOfOptionalHeader)
      bbuf.putShort(characteristics)
      bbuf.array()
    }
  }