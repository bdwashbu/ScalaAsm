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
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

class FactorialTest extends FlatSpec with ShouldMatchers {

  val executableName = "test_FactorialTest.exe"

  def getExecutable(input: Int) = {
    object Factorial extends AsmProgram[x86_32] {

      import com.scalaAsm.x86.Instructions.General._

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {

        builder += Code(
          mov(eax, dword(input)),
          mov(ebx, eax),
          label("Begin"),
          dec(ebx),
          test(ebx, ebx),
          je("End"),
          imul(ebx),
          jmp("Begin"),
          label("End"),
          push(eax),
          push("test"),
          call("printf"),
          pop(eax),
          pop(eax),
          retn(()))

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

        val output = in.readLine()

        child.waitFor()

        new File(executableName + input).delete()

        output should equal(output.toString)
    }

  }
}