package com.scalaAsm

import java.io._
import com.scalaAsm.portableExe.ExeGenerator
import com.scalaAsm.portableExe.OptionalHeader

object ScalaBasic {

  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));

      val beginTime = System.nanoTime()
      val assembled = HelloWorld.assemble
      val exe = ExeGenerator.compile(assembled, 0x2000)
      outputStream.write(exe.get)

      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")

      outputStream.close
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

}