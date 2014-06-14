package com.scalaAsm

import java.io._
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.scalaAsm.portableExe._
import com.scalaAsm.portableExe.sections.Sections
//import com.scalaAsm.portableExe.sections.ParsedImageResourceDirectory


object ShowResources {

  def main(args: Array[String]): Unit = {
    try {

      val file = new File("test.exe");
	 
	    val bFile: Array[Byte] = Array.fill(file.length().toInt)(0);
	      
	    //convert file into array of bytes
	    val fileInputStream = new FileInputStream(file);
	    fileInputStream.read(bFile);
	    fileInputStream.close();
	    
	    val bbuf = ByteBuffer.wrap(bFile)
	    bbuf.order(ByteOrder.LITTLE_ENDIAN)
	    
	    val dosHeader = DosHeader.getDosHeader(bbuf)
	    val peHeader = PeHeader.getPeHeader(bbuf)
	    val dirs = DataDirectories.getDirectories(bbuf)
	    val sections = Sections.getSections(bbuf, peHeader.fileHeader.numberOfSections)
      
//      val resourceRoot = ParsedImageResourceDirectory.getResources(bbuf, sections, dirs.resource)
//
//      println("TOP LEVEL: ")
//      println(ParsedImageResourceDirectory.getAllNamedEntries(resourceRoot).size)
//      ParsedImageResourceDirectory.getAllNamedEntries(resourceRoot).foreach(x => println(x.name))
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}