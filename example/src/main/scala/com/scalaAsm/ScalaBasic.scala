package com.scalaAsm

import java.io._

object ScalaBasic {

  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));

      val beginTime = System.nanoTime()
      val helloWorld = HelloWorld3.assemble
      helloWorld.addIcon("testicon.ico")
      val exe = helloWorld.link(0x2000, is64Bit = false, "kernel32.dll", "msvcrt.dll")

      outputStream.write(exe.get)

      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")

      outputStream.close
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}