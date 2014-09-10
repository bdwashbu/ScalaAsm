package com.scalaAsm

import java.io._
import com.scalaAsm.linker.{Linker32, Linker64}

object ScalaBasic {

  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));

      val beginTime = System.nanoTime()
      val helloWorld = HelloWorld3.assemble.addIcon("scala.ico")
      val exe = helloWorld.link(0x2000, new Linker32{}, "kernel32.dll", "msvcrt.dll")
      
      outputStream.write(exe.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream.close
      
      val outputStream64 = new DataOutputStream(new FileOutputStream("test64.exe"));
      
      val helloWorld64 = HelloWorld2.assemble
      val exe64 = helloWorld64.link(0x2000, new Linker64{}, "kernel32.dll", "msvcrt.dll")

      outputStream64.write(exe64.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream64.close
      
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}