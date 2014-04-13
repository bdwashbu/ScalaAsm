package com.scalaAsm.portableExe

import java.io.DataOutputStream
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer

private[portableExe] case class ImageDataDirectory(virtualAddress: Int, size: Int) extends ExeWriter {
  def write(stream: DataOutputStream) {
    write(stream, virtualAddress)
    write(stream, size)
  }
}

object DataDirectories {
  def getDirectories(input: ByteBuffer, numDirs: Int): Seq[ImageDataDirectory] = {
    for (i <- 0 until numDirs) yield ImageDataDirectory(input.getInt, input.getInt)
  }
}

object ImageExportDirectory {
  def getExports(input: ByteBuffer, sections: Seq[SectionHeader], dir: ImageDataDirectory): ImageExportDirectory = {
    
    val section = sections.find(section => section.virtualAddress <= dir.virtualAddress && 
                                           section.virtualAddress + section.sizeOfRawData > dir.virtualAddress)
    
    val exportFileOffset = section.get.virtualAddress - section.get.pointerToRawData 
    println(dir.virtualAddress - exportFileOffset)
    input.position(dir.virtualAddress - exportFileOffset)
    
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
	      while (input.get != '\0') {}
	      val name = Array.fill(input.position() - (RVA - exportFileOffset) - 1)(0.toByte)
	      input.position(RVA - exportFileOffset)
	      input.get(name)
	      name.map(_.toChar).mkString
	    }
      }
    }
  }
}

case class ImageExportDirectory(
    characteristics: Int,
    timeDateStamp: Int,
    majorVersion: Short,
    minorVersion: Short,
    name: Int,
    base: Int,
    numberOfFunctions: Int,
    numberOfNames: Int,
    addressOfFunctions: Int,
    addressOfNames: Int,
    addressOfNameOrdinals: Int) {
  def functionNames(): Seq[String] = Nil
}

// Contains the addresses of all important data structures in the PE

private[portableExe] case class DataDirectories(
    exportSymbols: Option[ImageDataDirectory] = None,
    importSymbols: Option[ImageDataDirectory] = None,
    resource: Option[ImageDataDirectory] = None,
    exception: Option[ImageDataDirectory] = None,
    security: Option[ImageDataDirectory] = None,
    baseRelocation: Option[ImageDataDirectory] = None,
    debug: Option[ImageDataDirectory] = None,
    copyRight: Option[ImageDataDirectory] = None,
    globalPtr: Option[ImageDataDirectory] = None,
    tls: Option[ImageDataDirectory] = None,
    loadConfig: Option[ImageDataDirectory] = None,
    boundImport: Option[ImageDataDirectory] = None,
    importAddressTable: Option[ImageDataDirectory] = None,
    com: Option[ImageDataDirectory] = None,
    delayedImport: Option[ImageDataDirectory] = None,
    reserved: Option[ImageDataDirectory] = None) extends Function0[Array[Byte]] {
    
    def size: Int = {
      15 * 8
    }
      
    def apply(): Array[Byte] = {
      val directoryOutput = new ByteArrayOutputStream()
      val stream = new DataOutputStream(directoryOutput)
      
      def write(dir: Option[ImageDataDirectory]) = dir match {
        case Some(dir) => dir.write(stream)
        case None => ImageDataDirectory(0,0).write(stream)
      }
      
      write(exportSymbols)
      write(importSymbols)
      write(resource)
      write(exception)
      write(security)
      write(baseRelocation)
      write(debug)
      write(copyRight)
      write(globalPtr)
      write(tls)
      write(loadConfig)
      write(boundImport)
      write(importAddressTable)
      write(com)
      write(delayedImport)
      write(reserved)
      
      directoryOutput.toByteArray
    }
  }