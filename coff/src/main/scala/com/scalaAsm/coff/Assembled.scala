package com.scalaAsm.coff

import java.io.File
import java.io.FileInputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder

object Assembled {
  def readCoff(filePath: String): Assembled = { 
    
    val file = new File(filePath);
 
    val bFile: Array[Byte] = Array.fill(file.length().toInt)(0);
      
    //convert file into array of bytes
    val fileInputStream = new FileInputStream(file);
    fileInputStream.read(bFile);
    fileInputStream.close();
    
    val bbuf = ByteBuffer.wrap(bFile)
    bbuf.order(ByteOrder.LITTLE_ENDIAN)
    
    val coffHeader = CoffHeader.getCoffHeader(bbuf)
    // only image files have an optional header
    val sectionHeaders = 
      for (i <- 0 until coffHeader.numberOfSections)
         yield SectionHeader.getSectionHeader(bbuf)
    
    println(coffHeader)
    sectionHeaders.foreach(println)
    null
  }
}

abstract class Assembled(val iconPath: Option[String] = None) {
  self =>
  val rawData: Array[Byte]
  val variables: Map[String, Int]
  val unboundSymbols: Seq[String]

  def finalizeAssembly(addressOfData: Int, imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte]

  def addIcon(path: String): Assembled = {
    new Assembled(Option(path)) {
      val rawData = self.rawData
      val variables = self.variables
      val unboundSymbols = self.unboundSymbols

      def finalizeAssembly(addressOfData: Int, imports: Map[String, Int], imports64: Map[String, Int], baseOffset: Int): Array[Byte] = {
        self.finalizeAssembly(addressOfData, imports, imports64, baseOffset)
      }
    }
  }
}