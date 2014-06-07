package com.scalaAsm

import java.io._
import com.scalaAsm.portableExe.ExeGenerator
import com.scalaAsm.portableExe.OptionalHeader

object ScalaBasic {

  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));

      val beginTime = System.nanoTime()
      val assembled = HelloWorld3.assemble
      val exe = ExeGenerator.link(assembled, 0x2000, is64Bit = false, "kernel32.dll", "msvcrt.dll")
      println(exe)
      outputStream.write(exe.get)

      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")

      outputStream.close
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}