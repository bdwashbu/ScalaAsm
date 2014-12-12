package com.scalaAsm

import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.coff.Coff

object ScalaBasic {

  case class x86InstructionDef(opcode: Int, mnemonic: String, operand1: String, operand2: String)
  
  def decodeOperandType(a: String, t: String): List[String] = {
    (a,t) match {
      case ("E", "b") => List("rm8")
      case ("E", "vqp") => List("r8", "r16", "r32", "r64")
      case ("G", "b") => List("r8")
      case ("G", "vqp") => List("rm8", "rm16", "rm32", "rm64")
      case _ => List()
    }
  }
  
  def loadXML() = {
    import scala.xml.XML
    val xml = XML.loadFile("x86reference.xml")
    val good = (xml \\ "pri_opcd").filter{x => (x \\ "syntax" \ "mnem").text == "ADD"}
    val defs = good.map{inst => 
      val dst = decodeOperandType((inst \\ "dst" \ "a").text, (inst \\ "dst" \ "t").text)
      val src = decodeOperandType((inst \\ "src" \ "a").text, (inst \\ "src" \ "t").text)
      val opcode = (inst \@ "value").toInt
      for {
        dstOp <- dst
        srcOp <- src
      } yield x86InstructionDef(opcode, "ADD", dstOp, srcOp)
      
    }
    defs.foreach(println)
  }
  
  
  def main(args: Array[String]): Unit = {
    try {

      loadXML()
      
      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
      val assembler = new Assembler {}
      val linker = new Linker{}

      var beginTime = System.nanoTime()
      val helloWorld = assembler.assemble(HelloWorld3).addIcon("scala.ico")
      
      val exe = linker.link(helloWorld, 0x3000, false, "kernel32.dll", "msvcrt.dll")
      
      outputStream.write(exe.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream.close
      
      val outputStreamSimple = new DataOutputStream(new FileOutputStream("testSimple.exe"));

      beginTime = System.nanoTime()
      val helloWorldSimple = assembler.assemble(HelloWorld).addIcon("scala.ico")
      helloWorldSimple.write("testCoff.obj")
      val exeSimple = linker.link(helloWorldSimple, 0x3000, false, "kernel32.dll", "msvcrt.dll")
      
      outputStreamSimple.write(exeSimple.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStreamSimple.close
      
      val outputStream64 = new DataOutputStream(new FileOutputStream("test64.exe"));
      
      val helloWorld64 = assembler.assemble(HelloWorld2).addIcon("scala.ico")
      val exe64 = linker.link(helloWorld64, 0x3000, true, "kernel32.dll", "msvcrt.dll")

      outputStream64.write(exe64.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream64.close
      
      val outputStream2 = new DataOutputStream(new FileOutputStream("test2.exe"));
      
      val coff = Coff.readCoff("helloWorld32.obj")
      val exe2 = linker.link(coff, 0x3000, false, "kernel32.dll", "msvcrt.dll")
      
      outputStream2.write(exe2.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream2.close
//      println(coff)
      
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}