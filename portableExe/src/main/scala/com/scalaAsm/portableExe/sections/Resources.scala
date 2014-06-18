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
    
    //convert file into array of bytes
    val fileInputStream = new FileInputStream(file);
    fileInputStream.read(bFile);
    fileInputStream.close();
    
    val bbuf = ByteBuffer.wrap(bFile)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    val icon = bbuf.array()
    icon(18) = 1 // the ordinal name has to be 1?
    
    // ROOT

    val first = IdResourceDirectoryEntry(DirectoryTypeID.icon.id, 
            ResourceDirectory(List(), List(IdResourceDirectoryEntry(
                1, ResourceDirectory(List(),List(LeafResourceDirectoryEntry(
                    0x409, ImageResourceDataEntry(ResourceData(icon.drop(22))))))))))
                    
    val second = IdResourceDirectoryEntry(DirectoryTypeID.groupIcon.id,
            ResourceDirectory(List(NamedResourceDirectoryEntry(
                ImageResourceDirString("MAINICON"), ResourceDirectory(List(),List(LeafResourceDirectoryEntry(
                    0x409, ImageResourceDataEntry(ResourceData(icon.take(20)))))))),List()))
    
    val res = RootDir(ResourceDirectory(List(),List(
        first,
        second
    )))
    
    res.place
    buffer.put(res.output)
  
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

//object ParsedImageResourceDirectory {
//
//  def getResourceDir(input: ByteBuffer, beginningFileLocation: Int, level: Int, beginningOfSection: Int): ParsedImageResourceDirectory = {
//    val characteristics = input.getInt
//    val timeDateStamp = input.getInt
//    val majorVersion = input.getShort
//    val minorVersion = input.getShort
//    val numberOfNamedEntries = input.getShort
//    val numberOfIdEntries = input.getShort
//
//    val namedEntries = for (i <- 0 until numberOfNamedEntries) yield {
//      ParsedImageResourceDirectoryEntry.getNamedDirectoryEntry(input, beginningFileLocation, beginningOfSection, level)
//    }
//
//    val idEntries = for (i <- 0 until numberOfIdEntries) yield {
//      ParsedImageResourceDirectoryEntry.getIdDirectoryEntry(input, beginningFileLocation, beginningOfSection, level)
//    }
//
//    ParsedImageResourceDirectory(
//      characteristics, 
//      timeDateStamp, 
//      majorVersion, 
//      minorVersion, 
//      namedEntries,
//      idEntries)
//  }
//
//  def getAllNamedEntries(root: ParsedImageResourceDirectory): Seq[NamedImageResourceDirectoryEntry] = {
//      val dirs = (root.namedEntries ++ root.idEntries).collect { case x: ParsedImageResourceDirectoryEntry => x }
//      dirs.collect { case x: NamedImageResourceDirectoryEntry => x } ++ dirs.flatMap { x => getAllNamedEntries(x.directory) }
//  }
//  
//  def getResources(input: ByteBuffer, sections: Seq[SectionHeader], dir: ImageDataDirectory): ParsedImageResourceDirectory = {
//
//    val section = sections.find(section => section.virtualAddress <= dir.virtualAddress &&
//      section.virtualAddress + section.sizeOfRawData > dir.virtualAddress)
//
//    val resourceFileOffset = section.get.virtualAddress - section.get.pointerToRawData
//    input.position(dir.virtualAddress - resourceFileOffset)
//    val beginningFileLocation = input.position
//
//    val resourceRoot = getResourceDir(input, beginningFileLocation, 0, resourceFileOffset)
//
//    resourceRoot
//  }
//}

case class ResourceData(data: Array[Byte]) extends ResourceField {
  
  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(data.size);
    buffer.put(data)
    buffer.array
  }
  
  def size = data.size
  def getChildren = Nil
}

case class ImageResourceDataEntry(data: ResourceData) extends ResourceField {

  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(16);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0x3000 + data.position)
    buffer.putInt(data.size)
    buffer.putInt(0)
    buffer.putInt(0)
    buffer.array
  }
  
  def size = 16
  def getChildren = List(data)
}

case class ImageResourceDirString(
  name: String) extends ResourceField {
  
  val length = name.size
  
  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(name.length*2 + 2);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putShort(length.toShort) // characteristics
    
    name.toCharArray().foreach{x => buffer.putChar(x)}
    buffer.array
  }
  
  def size = name.length*2 + 2
  def getChildren = Nil
}

trait ResourceField {
  var position = 0
  def apply: Array[Byte]
  def getChildren: List[ResourceField]
  def size: Int
  def getTotalSize: Int = size + getChildren.map(_.getTotalSize).sum
}

trait DirectoryEntry extends ResourceField {
  def size = 8
}

case class IdResourceDirectoryEntry (
  val id: Int,
  val childDir: ResourceDirectory) extends DirectoryEntry {
  
  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(8);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(id)
    buffer.putInt(0x80000000 + childDir.position)
    buffer.array
  }
  
  def getChildren = List(childDir)
}

case class LeafResourceDirectoryEntry (
  val name: Int,
  val data: ImageResourceDataEntry) extends DirectoryEntry {
  
  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(8);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(name)
    buffer.putInt(data.position)
    buffer.array
  }
  
  def getChildren = List(data)
}

case class NamedResourceDirectoryEntry (
  val name: ImageResourceDirString,
  val childDir: ResourceDirectory) extends DirectoryEntry {
  
  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(8);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0x80000000 + name.position)
    buffer.putInt(0x80000000 + childDir.position)
    buffer.array
  }
  
  override def size = 8
  def getChildren = List(name, childDir)
}

case class ResourceDirectory(
  namedEntries: List[DirectoryEntry],
  idEntries: List[DirectoryEntry]) extends ResourceField {
  
  def apply: Array[Byte] = {
    val buffer = ByteBuffer.allocate(16 + namedEntries.size*8 + idEntries.size*8);
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    buffer.putInt(0) // characteristics
    buffer.putInt(0) // time
    buffer.putShort(0) // major version
    buffer.putShort(0) // minor version
    buffer.putShort(namedEntries.size.toShort) // num named
    buffer.putShort(idEntries.size.toShort) // num id    
    namedEntries.foreach{entry => buffer.put(entry.apply)}  
    idEntries.foreach{entry => buffer.put(entry.apply)}
    buffer.array
  }

  def size = 16 + (namedEntries ++ idEntries).map(_.size).sum
             
  def getChildren = (namedEntries ++ idEntries).flatMap(_.getChildren)
}

case class RootDir(dir: ResourceDirectory) {
    
  def place: Unit = {
    def placeRecursive(resourceField: ResourceField, position: Int): Unit = {
      println(resourceField.getClass().getName() + ": " + position)
      var newPos = position + resourceField.size
      resourceField.position = position;
      resourceField.getChildren.foreach{child => placeRecursive(child, newPos); newPos += child.getTotalSize}
    }
    
    var newPos = 16
    dir.namedEntries.foreach{entry => entry.position = newPos; newPos += entry.size}
    dir.idEntries.foreach{entry => entry.position = newPos; newPos += entry.size}
    
    dir.getChildren.foreach{child => placeRecursive(child, newPos); newPos += child.getTotalSize;}
  }
  
  def output: Array[Byte] = {
    def outputRecursive(resourceField: ResourceField): Array[Byte] = {
      resourceField.apply ++ (if (resourceField.getChildren.size == 0) Array[Byte]() else resourceField.getChildren.map{child => outputRecursive(child)}.reduce(_++_))
    }
    
    dir.apply ++ (if (dir.getChildren.size == 0) Array[Byte]() else dir.getChildren.map{child => outputRecursive(child)}.reduce(_++_)) 
  }
}

case class ImageResourceDirectory(
  characteristics: Int,
  timeDateStamp: Int,
  majorVersion: Short,
  minorVersion: Short,
  namedEntries: Short,
  idEntries: Short){
  
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

//case class ParsedImageResourceDirectory(
//  characteristics: Int,
//  timeDateStamp: Int,
//  majorVersion: Short,
//  minorVersion: Short,
//  namedEntries: Seq[NamedImageResourceDirectoryEntry],
//  idEntries: Seq[ImageResourceEntry]) {
//}

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

//trait ParsedImageResourceDirectoryEntry extends ImageResourceEntry {
//  val offsetToData: Int
//  val directory: ParsedImageResourceDirectory
//}

//case class NamedImageResourceDirectoryEntry(
//  name: String,
//  offsetToData: Int,
//  directory: ParsedImageResourceDirectory) extends ParsedImageResourceDirectoryEntry
//
//case class IdImageResourceDirectoryEntry(
//  id: Int,
//  offsetToData: Int,
//  directory: ParsedImageResourceDirectory) extends ParsedImageResourceDirectoryEntry


  
object ParsedImageResourceDirectoryEntry {
//  def getNamedDirectoryEntry(input: ByteBuffer, beginningFileLocation: Int, beginningOfSection: Int, level: Int): NamedImageResourceDirectoryEntry = {
//    val namePtr = input.getInt
//    val offsetToData = input.getInt
//    val savedPos = input.position
//    //val result = if (namePtr >>> 31 == 1) {
//    val pointer = namePtr & 0x7FFFFFFF
//    input.position(beginningFileLocation + pointer)
//    val stringLength = input.getShort
//    var name: String = ""
//    for (i <- 0 until stringLength) {
//      name += input.getShort.toChar
//    }
//    var resDir: ParsedImageResourceDirectory = null
//
//    val savedPos2 = input.position
//    input.position((offsetToData & 0x7FFFFFFF) + beginningFileLocation)
//    if (level < 2) {
//      resDir = ParsedImageResourceDirectory.getResourceDir(input, beginningFileLocation, level + 1, beginningOfSection)
//    }
//    input.position(savedPos2)
//
//    val result = NamedImageResourceDirectoryEntry(name, offsetToData, resDir)
//    //}
//    input.position(savedPos)
//    result
//  }
//
//  def getIdDirectoryEntry(input: ByteBuffer, beginningFileLocation: Int, beginningOfSection: Int, level: Int): ImageResourceEntry = {
//    val namePtr = input.getInt
//    val offsetToData = input.getInt
//    val savedPos = input.position
//    val id = namePtr & 0x7FFFFFFF
//    var result: ImageResourceEntry = null
//
//    if (id == 0x409 || id == 0x415) {
//      input.position((offsetToData & 0x7FFFFFFF) + beginningFileLocation)
//      result = ImageResourceDataEntry.getDataEntry(input, beginningFileLocation)
//    } else {
//      val savedPos = input.position
//      input.position((offsetToData & 0x7FFFFFFF) + beginningFileLocation)
//      if (level < 2) {
//        val resDir = ParsedImageResourceDirectory.getResourceDir(input, beginningFileLocation, level + 1, beginningOfSection)
//        result = IdImageResourceDirectoryEntry(id, offsetToData, resDir)
//      }
//      input.position(savedPos)
//    }
//
//    input.position(savedPos)
//    result
//  }
}

object ImageResourceDataEntry {
  def getDataEntry(input: ByteBuffer, beginningOfSection: Int): ImageResourceDataEntry = {
    val offset = input.getInt
    val size = input.getInt
    val codePage = input.getInt
    val reserved = input.getInt
    
    

    input.position(offset + beginningOfSection)
    
    val data = Array.fill(size)(0.toByte)
    input.get(data)
    ImageResourceDataEntry(ResourceData(data))
  }
}
