package com.scalaAsm.portableExe

import java.nio.ByteBuffer
import java.nio.ByteOrder

// aka COFF Header

object FileHeader {
  def getFileHeader(input: ByteBuffer): FileHeader = {
    input.order(ByteOrder.LITTLE_ENDIAN)
    val header = new FileHeader {
        machine = input.getShort()
        numberOfSections = input.getShort()
    	timeDateStamp = input.getInt()
    	pointerToSymbolTable = input.getInt()
    	numberOfSymbols = input.getInt()
    	sizeOfOptionalHeader = input.getShort()
    	characteristics = input.getShort()
    }
   
    header
  }
}

class FileHeader {
    var machine: Short = 0x14C
    var numberOfSections: Short = 2
    var timeDateStamp: Int = 0x5132F2E5
    var pointerToSymbolTable: Int = _ // no importance
    var numberOfSymbols: Int = _ // no importance
    var sizeOfOptionalHeader: Short = 0x0E0
    var characteristics: Short = 271

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