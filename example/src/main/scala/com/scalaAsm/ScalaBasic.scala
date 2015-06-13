package com.scalaAsm

import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.coff.Coff
import scala.xml._
import java.io.PrintWriter

object ScalaBasic {

  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
      val assembler = new Assembler {}
      val linker = new Linker {}

      var beginTime = System.nanoTime()
      

      beginTime = System.nanoTime()
      

      val outputStream64 = new DataOutputStream(new FileOutputStream("test64.exe"));

      val helloWorld64 = assembler.assemble(HelloWorld2).addIcon("scala.ico")
      val exe64 = linker.link(helloWorld64, 0x3000, true, false, "kernel32.dll", "msvcrt.dll")

      outputStream64.write(exe64.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream64.close

      val outputStream2 = new DataOutputStream(new FileOutputStream("test2.exe"));

      val coff = Coff.readCoff("helloWorld32.obj")
      val exe2 = linker.link(coff, 0x3000, false, false, "kernel32.dll", "msvcrt.dll")

      outputStream2.write(exe2.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream2.close
      
      val rdtscStream = new DataOutputStream(new FileOutputStream("factorial.exe"));

      val rdtsc = assembler.assemble(rdtscExample).addIcon("scala.ico")
      val rdtscExe = linker.link(rdtsc, 0x3000, false, false, "kernel32.dll", "msvcrt.dll")

      rdtscStream.write(rdtscExe.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      rdtscStream.close
      
      
      //      println(coff)

    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}