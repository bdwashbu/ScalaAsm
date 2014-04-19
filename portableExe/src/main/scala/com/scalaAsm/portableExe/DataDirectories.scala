package com.scalaAsm.portableExe

import java.io.DataOutputStream
import scala.collection.immutable.TreeMap
import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.io.FileOutputStream

private[portableExe] case class ImageDataDirectory(virtualAddress: Int, size: Int) extends ExeWriter {
  def write(stream: DataOutputStream) {
    write(stream, virtualAddress)
    write(stream, size)
  }
}

object DataDirectories {
  def getDirectories(input: ByteBuffer): DataDirectories = {
    DataDirectories(
	    exportSymbols = ImageDataDirectory(input.getInt, input.getInt),
	    importSymbols = ImageDataDirectory(input.getInt, input.getInt),
	    resource = ImageDataDirectory(input.getInt, input.getInt),
	    exception = ImageDataDirectory(input.getInt, input.getInt),
	    security = ImageDataDirectory(input.getInt, input.getInt),
	    baseRelocation = ImageDataDirectory(input.getInt, input.getInt),
	    debug = ImageDataDirectory(input.getInt, input.getInt),
	    copyRight = ImageDataDirectory(input.getInt, input.getInt),
	    globalPtr = ImageDataDirectory(input.getInt, input.getInt),
	    tls = ImageDataDirectory(input.getInt, input.getInt),
	    loadConfig = ImageDataDirectory(input.getInt, input.getInt),
	    boundImport = ImageDataDirectory(input.getInt, input.getInt),
	    importAddressTable = ImageDataDirectory(input.getInt, input.getInt),
	    com = ImageDataDirectory(input.getInt, input.getInt),
	    delayedImport = ImageDataDirectory(input.getInt, input.getInt),
	    reserved = ImageDataDirectory(input.getInt, input.getInt))
  }
}

object ImageExportDirectory {
  def getExports(input: ByteBuffer, sections: Seq[SectionHeader], dir: ImageDataDirectory): ImageExportDirectory = {
    
    val section = sections.find(section => section.virtualAddress <= dir.virtualAddress && 
                                           section.virtualAddress + section.sizeOfRawData > dir.virtualAddress)
    
    val exportFileOffset = section.get.virtualAddress - section.get.pointerToRawData 
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

object ImageResourceDirectory {
  
  def getResourceDir(input: ByteBuffer, beginningFileLocation: Int, level: Int, beginningOfSection: Int): ImageResourceDirectory = {
    val characteristics = input.getInt
    val timeDateStamp = input.getInt
    val majorVersion = input.getShort
    val minorVersion = input.getShort
    val numberOfNamedEntries = input.getShort
	val numberOfIdEntries = input.getShort
	  
    val namedEntries = for (i <- 0 until numberOfNamedEntries) yield {
      val entry = ImageResourceDirectoryEntry.getNamedDirectoryEntry(input, beginningFileLocation)
      println("offset: " + entry.offsetToData)
      
      
      val savedPos = input.position
      input.position((entry.offsetToData & 0x7FFFFFFF) + beginningFileLocation)
      if (level < 2) {
    	  val resDir = getResourceDir(input, beginningFileLocation, level + 1, beginningOfSection)  
    	  println("what2: " + resDir)
      }
      input.position(savedPos)
      entry
    }
    
    val idEntries = for (i <- 0 until numberOfIdEntries) yield {
      val entry = ImageResourceDirectoryEntry.getIdDirectoryEntry(input, beginningFileLocation)
      
      if (entry.id == 0x409 || entry.id == 0x415) {
        input.position((entry.offsetToData & 0x7FFFFFFF) + beginningFileLocation)
        val data = ImageResourceDataEntry(
		    offsetToData = input.getInt,
		    size = input.getInt,
		    codePage = input.getInt,
		    reserved = input.getInt
		)
		val blah = Array.fill(data.size)(0.toByte)
		println(data.offsetToData - beginningFileLocation)
		input.position(data.offsetToData - beginningOfSection)
		input.get(blah)
		val outputStream = new DataOutputStream(new FileOutputStream("test2.cur"));
        outputStream.write(blah)
        outputStream.close
		println(data)
      } else if (entry.id == 2) {
	      val savedPos = input.position
	      input.position((entry.offsetToData & 0x7FFFFFFF) + beginningFileLocation)
	      if (level < 2) {
	    	  val resDir = getResourceDir(input, beginningFileLocation, level + 1, beginningOfSection)  
	    	  println("what1: " + resDir)
	      }
	      input.position(savedPos)
      }
      entry
    }
    
    ImageResourceDirectory(
	    characteristics,
	    timeDateStamp,
	    majorVersion,
	    minorVersion,
	    namedEntries,
	    idEntries
    )
  }
  
  def getResources(input: ByteBuffer, sections: Seq[SectionHeader], dir: ImageDataDirectory): ImageResourceDirectory = {
    
    
    val section = sections.find(section => section.virtualAddress <= dir.virtualAddress && 
                                           section.virtualAddress + section.sizeOfRawData > dir.virtualAddress)
    
    val resourceFileOffset = section.get.virtualAddress - section.get.pointerToRawData 
    input.position(dir.virtualAddress - resourceFileOffset)
    val beginningFileLocation = input.position
    
    getResourceDir(input, beginningFileLocation, 0, resourceFileOffset)
  }
}

case class ImageResourceDirString (
    length: Short,
    name: String
)

case class ImageResourceDirectory(
    characteristics: Int,
    timeDateStamp: Int,
    majorVersion: Short,
    minorVersion: Short,
    namedEntries: Seq[ImageResourceDirectoryEntry],
    idEntries: Seq[ImageResourceDirectoryEntry]
)

object ImageResourceDirectoryEntry {
  def getNamedDirectoryEntry(input: ByteBuffer, beginningFileLocation: Int): NamedImageResourceDirectoryEntry = {
    val namePtr = input.getInt
    val offsetToData = input.getInt
    val savedPos = input.position
    //val result = if (namePtr >>> 31 == 1) {
      val pointer =  namePtr & 0x7FFFFFFF
      input.position(beginningFileLocation + pointer)
      val stringLength = input.getShort
      var name: String = ""
      for (i <- 0 until stringLength) {
        name += input.getShort.toChar
      }
      val result = NamedImageResourceDirectoryEntry(name, offsetToData)
   //}
    input.position(savedPos)
    result
  }
  
  def getIdDirectoryEntry(input: ByteBuffer, beginningFileLocation: Int): IdImageResourceDirectoryEntry = {
    val namePtr = input.getInt
    val offsetToData = input.getInt
    val savedPos = input.position
    val id = namePtr & 0x7FFFFFFF
    val result = IdImageResourceDirectoryEntry(id, offsetToData)
    input.position(savedPos)
    result
  }
}

trait ImageResourceDirectoryEntry {
  val offsetToData: Int
}

case class NamedImageResourceDirectoryEntry (
    name: String,
    offsetToData: Int
) extends ImageResourceDirectoryEntry

case class IdImageResourceDirectoryEntry (
    id: Int,
    offsetToData: Int
) extends ImageResourceDirectoryEntry

case class ImageResourceDataEntry(
    offsetToData: Int,
    size: Int,
    codePage: Int,
    reserved: Int
) extends ImageResourceDirectoryEntry

object ResourceType extends Enumeration {
  type ResourceType = Value
  val cursor = Value(1)
  val bitmap = Value(2)
  val icon = Value(3)
  val menu = Value(4)
  val dialog = Value(5)
  val string = Value(6)
  val fontdir = Value(7)
  val font = Value(8)
  val accelerator = Value(9)
  val rcData = Value(10)
  val messageTable = Value(11)
}

// Contains the addresses of all important data structures in the PE

private[portableExe] case class DataDirectories(
    exportSymbols: ImageDataDirectory = ImageDataDirectory(0,0),
    importSymbols: ImageDataDirectory = ImageDataDirectory(0,0),
    resource: ImageDataDirectory = ImageDataDirectory(0,0),
    exception: ImageDataDirectory = ImageDataDirectory(0,0),
    security: ImageDataDirectory = ImageDataDirectory(0,0),
    baseRelocation: ImageDataDirectory = ImageDataDirectory(0,0),
    debug: ImageDataDirectory = ImageDataDirectory(0,0),
    copyRight: ImageDataDirectory = ImageDataDirectory(0,0),
    globalPtr: ImageDataDirectory = ImageDataDirectory(0,0),
    tls: ImageDataDirectory = ImageDataDirectory(0,0),
    loadConfig: ImageDataDirectory = ImageDataDirectory(0,0),
    boundImport: ImageDataDirectory = ImageDataDirectory(0,0),
    importAddressTable: ImageDataDirectory = ImageDataDirectory(0,0),
    com: ImageDataDirectory = ImageDataDirectory(0,0),
    delayedImport: ImageDataDirectory = ImageDataDirectory(0,0),
    reserved: ImageDataDirectory = ImageDataDirectory(0,0)) extends Function0[Array[Byte]] {
    
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
      
      exportSymbols.write(stream)
      importSymbols.write(stream)
      resource.write(stream)
      exception.write(stream)
      security.write(stream)
      baseRelocation.write(stream)
      debug.write(stream)
      copyRight.write(stream)
      globalPtr.write(stream)
      tls.write(stream)
      loadConfig.write(stream)
      boundImport.write(stream)
      importAddressTable.write(stream)
      com.write(stream)
      delayedImport.write(stream)
      reserved.write(stream)
      
      directoryOutput.toByteArray
    }
  }