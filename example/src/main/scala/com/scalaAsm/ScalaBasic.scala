package com.scalaAsm

import java.io._
import com.scalaAsm.linker.{Linker64}
import com.scalaAsm.assembler.Assembler

object ScalaBasic {

  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
      val assembler = new Assembler {}
      val linker = new Linker64{}

      val beginTime = System.nanoTime()
      val helloWorld = assembler.assemble(HelloWorld3)
      val exe = linker.link(helloWorld, 0x3000, false, "kernel32.dll", "msvcrt.dll")
      
      outputStream.write(exe.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream.close
      
      val outputStream64 = new DataOutputStream(new FileOutputStream("test64.exe"));
      
      val helloWorld64 = assembler.assemble(HelloWorld2)
      val exe64 = linker.link(helloWorld64, 0x3000, true, "kernel32.dll", "msvcrt.dll")

      outputStream64.write(exe64.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream64.close
      
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}