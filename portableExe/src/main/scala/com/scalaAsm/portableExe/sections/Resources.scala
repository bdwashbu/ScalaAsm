package com.scalaAsm.portableExe
package sections

import java.nio.ByteBuffer
import com.scalaAsm.portableExe.ImageDataDirectory
import java.io.File
import java.nio.ByteOrder
import java.io.FileInputStream

object ResourceGen {
  def compileResources(beginningOfSection: Int): Array[Byte] = { 
    
    val file = new File("testicon.ico");
	 
    
    val bFile: Array[Byte] = Array.fill(file.length().toInt)(0);
    
    val buffer = ByteBuffer.allocate(21300)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    
    // ROOT

    buffer.put(ImageResourceDirectory(0,0,0,0,0,2).apply)
    
    buffer.put(ImageResourceDirectoryEntry(DirectoryTypeID.icon.id, 0x80000000 + 16 + 8 + 8 + 16 + 8 + 16 + 8 + 16).apply)
    buffer.put(ImageResourceDirectoryEntry(DirectoryTypeID.groupIcon.id, 0x80000000 + 16 + 8 + 8).apply)
    
    buffer.put(ImageResourceDirectory(0,0,0,0,1,0).apply)
    
    buffer.put(ImageResourceDirectoryEntry(0x80000000 + 16 + 8 + 8 + 16 + 8 + 16 + 8 + 16 + 8 + 16 + 8 + 16 + 16,
                                           0x80000000 + 16 + 8 + 8 + 16 + 8).apply)
    
    buffer.put(ImageResourceDirectory(0,0,0,0,0,1).apply)
    
    buffer.put(ImageResourceDirectoryEntry(0x409, 16 + 8 + 8 + 16 + 8 + 16 + 8).apply)

    buffer.put(ImageResourceDataEntry(0x3000 + 16 + 8 + 8 + 16 + 8 + 16 + 8 + 16 + 18 + 8 + 16 + 8 + 16 + 16, 20).apply)
    
    buffer.put(ImageResourceDirectory(0,0,0,0,0,1).apply)
    
    buffer.put(ImageResourceDirectoryEntry(1, 0x80000000 + 16 + 8 + 8 + 16 + 8 + 16 + 8 + 16 + 8 + 16).apply)
    
    buffer.put(ImageResourceDirectory(0,0,0,0,0,1).apply)
    
    buffer.put(ImageResourceDirectoryEntry(1, 16 + 8 + 8 + 16 + 8 + 16 + 8 + 16 + 8 + 16 + 8 + 16).apply)

    buffer.put(ImageResourceDataEntry(0x3000 + 16 + 8 + 8 + 16 + 8 + 16 + 8 + 16 + 18 + 8 + 16 + 8 + 16 + 16 + 22, file.length().toInt - 22).apply)
    
    buffer.put(ImageResourceDirString(8,"MAINICON").apply)
      
    //convert file into array of bytes
    val fileInputStream = new FileInputStream(file);
    fileInputStream.read(bFile);
    fileInputStream.close();
    
    val bbuf = ByteBuffer.wrap(bFile)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    val icon = bbuf.array()
    icon(18) = 1 // the ordinal name has to be 1?
    buffer.put(icon)
    buffer.array()
  }
}

object DirectoryTypeID extends Enumeration {
    type DirType = Value
    val cursor = Value(1)
    val bitmap = Value(2)
    val icon = Value(3)
    val menu = Value(4)
    val dialogBox = Value(5)
    val stringTableEntry = Value(6)
    val fontDirectory = Value(7)
    val font = Value(8)
    val acceleratorTable = Value(9)
    val applicationDefinedResource = Value(10)
    val messageTableEntry = Value(11)
    val groupCursor = Value(12)
    val groupIcon = Value(14)
    val versionInformation = Value(16)
    val dlginclude = Value(17)
    val plugAndPlay = Value(19)
    val VXD = Value(20)
    val animatedCursor = Value(21)
    val animatedIcon = Value(22)
    val html = Value(23)
    val assemblyManifest = Value(24)
}

object ParsedImageResourceDirectory {

  def getResourceDir(input: ByteBuffer, beginningFileLocation: Int, level: Int, beginningOfSection: Int): ParsedImageResourceDirectory = {
    val characteristics = input.getInt
    val timeDateStamp = input.getInt
    val majorVersion = input.getShort
    val minorVersion = input.getShort
    val numberOfNamedEntries = input.getShort
    val numberOfIdEntries = input.getShort

    val namedEntries = for (i <- 0 until numberOfNamedEntries) yield {
      ParsedImageResourceDirectoryEntry.getNamedDirectoryEntry(input, beginningFileLocation, beginningOfSection, level)
    }

    val idEntries = for (i <- 0 until numberOfIdEntries) yield {
      ParsedImageResourceDirectoryEntry.getIdDirectoryEntry(input, beginningFileLocation, beginningOfSection, level)
    }

    ParsedImageResourceDirectory(
      characteristics, 
      timeDateStamp, 
      majorVersion, 
      minorVersion, 
      namedEntries,
      idEntries)
  }

  def getAllNamedEntries(root: ParsedImageResourceDirectory): Seq[NamedImageResourceDirectoryEntry] = {
      val dirs = (root.namedEntries ++ root.idEntries).collect { case x: ParsedImageResourceDirectoryEntry => x }
      dirs.collect { case x: NamedImageResourceDirectoryEntry => x } ++ dirs.flatMap { x => getAllNamedEntries(x.directory) }
  }
  
  def getResources(input: ByteBuffer, sections: Seq[SectionHeader], dir: ImageDataDirectory): ParsedImageResourceDirectory = {

    val section = sections.find(section => section.virtualAddress <= dir.virtualAddress &&
      section.virtualAddress + section.sizeOfRawData > dir.virtualAddress)

    val resourceFileOffset = section.get.virtualAddress - section.get.pointerToRawData
    input.position(dir.virtualAddress - resourceFileOffset)
    val beginningFileLocation = input.position

    val resourceRoot = getResourceDir(input, beginningFileLocation, 0, resourceFileOffset)

    resourceRoot
  }
}

case class ImageResourceDataEntry(offsetToData: Int, size: Int) extends ImageResourceEntry{
  def apply(): Array[Byte] = {
    val buffer = ByteBuffer.allocate(16);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(offsetToData)
    buffer.putInt(size)
    buffer.putInt(0)
    buffer.putInt(0)

    buffer.array
  }
}

case class ImageResourceDirString(
  length: Short,
  name: String) {
  
  def apply(): Array[Byte] = {
    val buffer = ByteBuffer.allocate(name.length*2 + 2);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putShort(length.toShort) // characteristics
    
    name.toCharArray().foreach{x => buffer.putChar(x)}
    buffer.array
  }
}

case class ImageResourceDirectory(
  characteristics: Int,
  timeDateStamp: Int,
  majorVersion: Short,
  minorVersion: Short,
  namedEntries: Short,
  idEntries: Short) {
  
  def apply(): Array[Byte] = {
    val buffer = ByteBuffer.allocate(16);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0) // characteristics
    buffer.putInt(0) // time
    buffer.putShort(0) // major version
    buffer.putShort(0) // minor version
    buffer.putShort(namedEntries) // num named
    buffer.putShort(idEntries) // num id
    buffer.array()
  }
}

case class ParsedImageResourceDirectory(
  characteristics: Int,
  timeDateStamp: Int,
  majorVersion: Short,
  minorVersion: Short,
  namedEntries: Seq[NamedImageResourceDirectoryEntry],
  idEntries: Seq[ImageResourceEntry]) {
}

case class ImageResourceDirectoryEntry(
  val name: Int,
  val offsetToData: Int) {
  
  def apply(): Array[Byte] = {
    val buffer = ByteBuffer.allocate(8);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(name)
    buffer.putInt(offsetToData)
    buffer.array
  }
}

trait ImageResourceEntry {
  val offsetToData: Int
}

trait ParsedImageResourceDirectoryEntry extends ImageResourceEntry {
  val offsetToData: Int
  val directory: ParsedImageResourceDirectory
}

case class NamedImageResourceDirectoryEntry(
  name: String,
  offsetToData: Int,
  directory: ParsedImageResourceDirectory) extends ParsedImageResourceDirectoryEntry

case class IdImageResourceDirectoryEntry(
  id: Int,
  offsetToData: Int,
  directory: ParsedImageResourceDirectory) extends ParsedImageResourceDirectoryEntry


  
object ParsedImageResourceDirectoryEntry {
  def getNamedDirectoryEntry(input: ByteBuffer, beginningFileLocation: Int, beginningOfSection: Int, level: Int): NamedImageResourceDirectoryEntry = {
    val namePtr = input.getInt
    val offsetToData = input.getInt
    val savedPos = input.position
    //val result = if (namePtr >>> 31 == 1) {
    val pointer = namePtr & 0x7FFFFFFF
    input.position(beginningFileLocation + pointer)
    val stringLength = input.getShort
    var name: String = ""
    for (i <- 0 until stringLength) {
      name += input.getShort.toChar
    }
    var resDir: ParsedImageResourceDirectory = null

    val savedPos2 = input.position
    input.position((offsetToData & 0x7FFFFFFF) + beginningFileLocation)
    if (level < 2) {
      resDir = ParsedImageResourceDirectory.getResourceDir(input, beginningFileLocation, level + 1, beginningOfSection)
    }
    input.position(savedPos2)

    val result = NamedImageResourceDirectoryEntry(name, offsetToData, resDir)
    //}
    input.position(savedPos)
    result
  }

  def getIdDirectoryEntry(input: ByteBuffer, beginningFileLocation: Int, beginningOfSection: Int, level: Int): ImageResourceEntry = {
    val namePtr = input.getInt
    val offsetToData = input.getInt
    val savedPos = input.position
    val id = namePtr & 0x7FFFFFFF
    var result: ImageResourceEntry = null

    if (id == 0x409 || id == 0x415) {
      input.position((offsetToData & 0x7FFFFFFF) + beginningFileLocation)
      result = ImageResourceDataEntry.getDataEntry(input, beginningFileLocation)
    } else {
      val savedPos = input.position
      input.position((offsetToData & 0x7FFFFFFF) + beginningFileLocation)
      if (level < 2) {
        val resDir = ParsedImageResourceDirectory.getResourceDir(input, beginningFileLocation, level + 1, beginningOfSection)
        result = IdImageResourceDirectoryEntry(id, offsetToData, resDir)
      }
      input.position(savedPos)
    }

    input.position(savedPos)
    result
  }
}

object ImageResourceDataEntry {
  def getDataEntry(input: ByteBuffer, beginningOfSection: Int): ImageResourceDataEntry = {
    val offset = input.getInt
    val size = input.getInt
    val codePage = input.getInt
    val reserved = input.getInt
    
    println("data " + (beginningOfSection))

    input.position(offset + beginningOfSection)
    
    //val data = Array.fill(size)(0.toByte)
    //input.get(data)
    ImageResourceDataEntry(
      offset,
      offset + beginningOfSection)
  }
}
