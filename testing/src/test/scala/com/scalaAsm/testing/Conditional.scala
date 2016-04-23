package com.scalaAsm.testing

import org.scalatest._
import java.io.DataOutputStream
import java.io.FileOutputStream
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream
import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import com.scalaAsm.x86._

class ConditionalTest extends FlatSpec with ShouldMatchers {

  val executableName = "test_ConditionalTest.exe"

  def getExecutable(input: Int) = {
    object Factorial extends AsmProgram {

      import com.scalaAsm.x86.Instructions.General._
      import com.scalaAsm.asm._
      import com.scalaAsm.x86.Instructions._
      
      sections += new DataSection(
        Variable("jumpTaken", "Jump taken!\n\u0000"),
        Variable("jumpNotTaken", "Jump not taken!\n\u0000")
      ) {}

      sections += new CodeSection {

        builder += Code(
          asm"""mov ebx, 4
          test ebx, $input
          jz zero
          jmp notzero
          zero:
          push jumpTaken
          call printf
          jmp end
          notzero:
          push jumpNotTaken
          call printf
          jmp end
          end:
          pop eax
          retn""")

      }
    }

    val fileOut = new FileOutputStream(executableName + input)
    val outputStream = new DataOutputStream(fileOut);
    val assembler = new Assembler {}
    val linker = new Linker {}
    
    val factorial = assembler.assemble(Factorial).addIcon("scala.ico")
    val exe = linker.link(factorial, 0x3000, false, false, "kernel32.dll", "msvcrt.dll")
    outputStream.write(exe.get)
    outputStream.close
    fileOut.close()
  }

  "jz" should "jump" in {
    val name = System.nanoTime
    
    getExecutable(0)
    val child = Runtime.getRuntime().exec(executableName + "0");
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val outputLine = in.readLine()

    child.waitFor()

    new File(executableName + "0").delete()

    outputLine should equal("Jump taken!")

  }
  
  "jz" should "not jump" in {
    val name = System.nanoTime
    
    getExecutable(4)
    val child = Runtime.getRuntime().exec(executableName + "4");
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val outputLine = in.readLine()

    child.waitFor()

    new File(executableName + "4").delete()

    outputLine should equal("Jump not taken!")

  }
}