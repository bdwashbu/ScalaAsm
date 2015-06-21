package com.scalaAsm

import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.coff.Coff
import com.scalaAsm.asm._
import scala.xml._
import java.io.PrintWriter
import scala.util.Try
import scala.collection.mutable.ListBuffer
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._

object HelloWorld extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._

  import com.scalaAsm.asm._
  import universe._
  
  sections += new DataSection(
    Variable("helloWorld", "Hello World!\r\n\u0000"),
    Variable("helloWorld2", "Hello Worldddd!\r\n\u0000")
  ) {}

  sections += new CodeSection {
    builder += Code(
      asm"""push helloWorld
      call printf
      pop ebx
      retn"""
    )
  }
}

object FragileEXE {

  def main(args: Array[String]): Unit = {
    

      
      val assembler = new Assembler {}
      val linker = new Linker {}

      val helloWorld = assembler.assemble(HelloWorld).addIcon("scala.ico")
      val exe = linker.link(helloWorld, 0x3000, false, false, "kernel32.dll", "msvcrt.dll")

      val data = exe.get
      
      val failureBits = ListBuffer[Int]()
      
      var beginTime = System.nanoTime()
      
      if (!new File("fragile").exists())
        new File("fragile").mkdir()
      
      (0 to 1).foreach { byteNum =>
        
        val originalByte: Byte = data(byteNum)
        
        failureBits ++= (0 to 7).par.map { bitNum =>
          
          val fileName = s"fragile/fragileTest${byteNum}_$bitNum.exe"
          
          
          val bitValue = ((originalByte) << (7 - bitNum) & 0xFF) >> 7
          
          val newByte: Byte = if (bitValue == 1) {
            (originalByte - math.pow(2,bitNum).toInt).toByte
          } else {
            (originalByte + math.pow(2,bitNum).toInt).toByte
          }
          
          val bitSwapped = data.patch(byteNum, Array[Byte](newByte), 1)
          val outputStream = new DataOutputStream(new FileOutputStream(fileName));
          outputStream.write(bitSwapped)
          outputStream.close
          
          var returnVal: Int = 0

          try {
            val child = Runtime.getRuntime().exec(fileName);
          
            var timer = 0
            while (timer < 150) {
              Thread.sleep(5)
              val test  = Try(child.exitValue())
              timer += 10
              if (test.isSuccess)
                timer = 150
              else if (timer == 150)
                returnVal = 1//Some(byteNum * 8 + bitNum)
            }
            
            if (timer < 150) {
              val in = new BufferedReader(
                new InputStreamReader(child.getInputStream()));
          
              val output = in.readLine()
          
              child.waitFor()
              in.close
            }
            
            child.destroy()
          } catch {
            case e: Exception => returnVal = 1//Some(byteNum * 8 + bitNum)
          }
//          while (new File(fileName).exists)
//            new File(fileName).delete()
            
          returnVal
        }.toList
      }
      
      println("done testing bits in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      println(failureBits.toList)
      println(failureBits.size)
      println(failureBits.map{x => (x / 8).toInt}.toSet.toList.sorted)
      //      println(coff)

    
    
    
  }

}