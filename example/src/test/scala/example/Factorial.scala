package example

import org.scalatest._
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86._

class FactorialTest extends FlatSpec with ShouldMatchers {

  val executableName = "test_FactorialTest.exe"
  
  def getExecutable(input: Int): AsmProgram = {
    new AsmProgram {

      import com.scalaAsm.x86.Instructions.General._
      import com.scalaAsm.x86.Operands._

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {

        builder += Code(
          asm"""mov eax, $input
          mov ebx, eax
          Begin:
          dec ebx
          test ebx, ebx
          je End
          imul ebx
          jmp Begin
          End:
          push eax
          push test
          call printf
          pop eax
          pop eax
          retn"""
        )

      }
    }
  }

  "A simple factorial test" should "print the correct results" in {
    val name = System.nanoTime
    
    def factorial(n:Int):Int = if (n==0) 1 else n * factorial(n-1)
    
    (1 to 12).foreach {
      input =>
        val expectedOutput = factorial(input)
        println(expectedOutput)
        getProgramOutput(getExecutable(input), false) should equal(expectedOutput.toString)
    }

  }
}