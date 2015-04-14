package example

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
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

class FactorialTest extends FlatSpec with ShouldMatchers {

  val executableName = "test_FactorialTest.exe"

  def getExecutable(input: Int) = {
    object Factorial extends AsmProgram[x86_32] {

      import com.scalaAsm.x86.Instructions.General._
      import com.scalaAsm.x86.Operands._
      
      val convertedInput: String = input.toString

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {

        builder += Code(
          asm"mov eax, $convertedInput",
          asm"mov ebx, eax",
          asm"Begin:",
          asm"dec ebx",
          asm"test ebx, ebx",
          asm"je End",
          asm"imul ebx",
          asm"jmp Begin",
          asm"End:",
          asm"push eax",
          asm"push test",
          asm"call printf",
          asm"pop eax",
          asm"pop eax",
          asm"retn"
        )

      }
    }

    val fileOut = new FileOutputStream(executableName + input)
    val outputStream = new DataOutputStream(fileOut);
    val assembler = new Assembler {}
    val linker = new Linker {}
    
    val factorial = assembler.assemble(Factorial).addIcon("scala.ico")
    val exe = linker.link(factorial, 0x3000, false, "kernel32.dll", "msvcrt.dll")
    outputStream.write(exe.get)
    outputStream.close
    fileOut.close()
  }

  "A simple factorial test" should "print the correct results" in {
    val name = System.nanoTime
    
    List((1, 1), (2, 2), (3, 6), (4, 24), (5, 120)).foreach {
      case (input, output) =>
        getExecutable(input)
        val child = Runtime.getRuntime().exec(executableName + input);
        val in = new BufferedReader(
          new InputStreamReader(child.getInputStream()));

        val outputLine = in.readLine()

        child.waitFor()

        new File(executableName + input).delete()

        outputLine should equal(output.toString)
    }

  }
}