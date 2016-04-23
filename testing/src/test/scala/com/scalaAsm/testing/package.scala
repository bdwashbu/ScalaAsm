package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import com.scalaAsm.asm.x86_32
import org.scalatest._
import scala.util.Try

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

package object testing {
  
  def getProgramOutput(program: AsmProgram, is64Bit: Boolean): String = {
    
    var output = ""

    val executableName = s"test${sync.getTestID}.exe"

    val name = System.nanoTime
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}  

    var beginTime = System.nanoTime()
    val obj = assembler.assemble(program).addIcon("scala.ico")

    val exe = linker.link(obj, 0x3000, is64Bit, false, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec(executableName);
    
    var timer = 0
    while (timer < 150) {
      Thread.sleep(10)
      val test  = Try(child.exitValue())
      timer += 10
      if (test.isSuccess)
        timer = 150
      else if (timer == 150)
        throw new Exception("Test took too long!")
    }
    
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    output = in.readLine()

    child.waitFor()

    new File(executableName).delete()
    output
  }
  
  def getDLLOutput(dllProgram: AsmProgram, program: AsmProgram, is64Bit: Boolean): String = {
    
    var output = ""

    val dllName = s"test${sync.getTestID}.dll"
    val executableName = s"test${sync.getTestID}.exe"

    val outputStream = new DataOutputStream(new FileOutputStream(dllName));
    val assembler = new Assembler {}
    val linker = new Linker {}  

    var beginTime = System.nanoTime()
    val dllobj = assembler.assemble(dllProgram)

    val dll = linker.link(dllobj, 0x3000, is64Bit, true)

    outputStream.write(dll.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close
    
    val outputStream2 = new DataOutputStream(new FileOutputStream(executableName));
    val assembler2 = new Assembler {}
    val linker2 = new Linker {}  

    beginTime = System.nanoTime()
    val exeobj = assembler.assemble(program)

    val exe = linker.link(exeobj, 0x3000, is64Bit, false, dllName, "kernel32.dll", "msvcrt.dll")

    outputStream2.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream2.close

    val child = Runtime.getRuntime().exec(executableName);
    
    Thread.sleep(500)
    try {
      child.exitValue()
    } catch {
      case exception: IllegalThreadStateException => throw new Exception("Test took too long!")
    }
    
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    output = in.readLine()

    child.waitFor()

    new File(executableName).delete()
    new File(dllName).delete()
    output
  }
  
}