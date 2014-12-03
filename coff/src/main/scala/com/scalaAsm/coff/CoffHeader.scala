package com.scalaAsm.coff

import java.nio.ByteBuffer
import java.nio.ByteOrder

abstract class CoffCharacteristic(val value: Short)
case object IMAGE_FILE_32BIT_MACHINE extends CoffCharacteristic(0x0100.toShort)

object CoffHeader {
  def getCoffHeader(input: ByteBuffer): CoffHeader = {
    input.order(ByteOrder.LITTLE_ENDIAN)
    val header = CoffHeader(
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

case class CoffHeader(
    machine: Short,
    numberOfSections: Short,
    timeDateStamp: Int,
    pointerToSymbolTable: Int, // no importance
    numberOfSymbols: Int, // no importance
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