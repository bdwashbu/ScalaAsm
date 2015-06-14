package com.scalaAsm.portableExe.sections

import java.nio.ByteBuffer
import java.nio.ByteOrder._
import java.nio.ByteOrder
import com.scalaAsm.portableExe.ImageDataDirectory
import com.scalaAsm.coff._

object ImageExportDirectory {
  
  val sizeOfHeader = 40
  
  def getExports(input: ByteBuffer, sections: Seq[SectionHeader], directoryAddress: Int): ImageExportDirectory = {
    
    val section = sections.find(section => section.virtualAddress <= directoryAddress && 
                                           section.virtualAddress + section.sizeOfRawData > directoryAddress)
    
    val exportFileOffset = section.get.virtualAddress - section.get.pointerToRawData 
    input.position(directoryAddress - exportFileOffset)
    
    new ImageExportDirectory(
    characteristics = input.getInt,
    timeDateStamp = input.getInt,
    majorVersion = input.getShort,
    minorVersion = input.getShort,
    name = input.getInt,
    base = input.getInt,
    numberOfFunctions = input.getInt,
    numberOfNames = input.getInt,
    addressOfFunctions = input.getInt,
    addressOfNames = input.getInt,
    addressOfNameOrdinals = input.getInt) {
      override def functionNames = { 
  	    for (i <- 0 until numberOfNames) yield {
  	      input.position(addressOfNames - exportFileOffset + i*4)
  	      val RVA = input.getInt
  	      input.position(RVA - exportFileOffset)
  	      while (input.get != '\u0000') {}
  	      val name = Array.fill(input.position() - (RVA - exportFileOffset) - 1)(0.toByte)
  	      input.position(RVA - exportFileOffset)
  	      input.get(name)
  	      name.map(_.toChar).mkString
  	    }
      }
    }
  }
  
  def writeExports(dllName: String, exports: Seq[Export], offset: Int): Array[Byte] = { 
    
    val rvaArray = exports.map(_.address)
    val rvaArrayAddress = offset + ImageExportDirectory.sizeOfHeader
    val nameArray = exports.map(_.name.toCharArray().map(_.toByte) :+ 0.toByte).reduce(_ ++ _)
    val nameArrayAddress = rvaArrayAddress + nameArray.size
    
    val dir = ImageExportDirectory(
      characteristics = 0,
      timeDateStamp = 0,
      majorVersion = 0,
      minorVersion = 0,
      name = 0,
      base = 1,
      numberOfFunctions = exports.size,
      numberOfNames = exports.size,
      addressOfFunctions = rvaArrayAddress,
      addressOfNames = nameArrayAddress,
      addressOfNameOrdinals = 0)
      
    val bbuf = ByteBuffer.allocate(ImageExportDirectory.sizeOfHeader + rvaArray.size * 4 + nameArray.size)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    
    bbuf.put(dir())
    rvaArray.foreach(bbuf.putInt(_))
    bbuf.put(nameArray)
    bbuf.array() 
  }
}

case class Export(
    name: String,
    address: Int
)

case class ImageExportDirectory(
    characteristics: Int,
    timeDateStamp: Int,
    majorVersion: Short,
    minorVersion: Short,
    name: Int, // name of the dll
    base: Int,
    numberOfFunctions: Int,
    numberOfNames: Int,
    addressOfFunctions: Int,
    addressOfNames: Int,
    addressOfNameOrdinals: Int) {
  def functionNames(): Seq[String] = Nil
  
  def apply() = {
      val bbuf = ByteBuffer.allocate(40);
      bbuf.order(ByteOrder.LITTLE_ENDIAN)
      bbuf.putInt(characteristics)
      bbuf.putInt(timeDateStamp)
      bbuf.putShort(majorVersion)
      bbuf.putShort(minorVersion)
      bbuf.putInt(name)
      bbuf.putInt(base)
      bbuf.putInt(numberOfFunctions)
      bbuf.putInt(numberOfNames)
      bbuf.putInt(addressOfFunctions)
      bbuf.putInt(addressOfNames)
      bbuf.putInt(addressOfNameOrdinals)
      bbuf.array()
    }
}