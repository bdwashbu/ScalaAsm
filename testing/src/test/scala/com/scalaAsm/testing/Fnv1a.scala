package com.scalaAsm.testing

import org.scalatest._
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86._
import com.scalaAsm.x86.Operands.Memory.Indirect

class Fnv1a extends FlatSpec with ShouldMatchers {

  val executableName = "test_FactorialTest.exe"
  
  def getExecutable(input: Int): AsmProgram = {
    new AsmProgram {

      import com.scalaAsm.x86.Instructions.General._
      import com.scalaAsm.x86.Instructions._

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {

//        builder += Code(
//          asm"""
//          push ebx
//          push esi
//          push edi
//          mov esi, [esp + 16] //buffer
//          mov ecx, [esp + 20] //length
//          mov eax, [esp + 24] //basis
//          mov edi, 0x01000193 //fnv_32_prime
//          xor ebx, ebx
//        nexta:
//          mov bl, [esi]
//          xor eax, ebx
//          mul edi
//          inc esi
//          dec ecx
//          jnz nexta
//          pop edi
//          pop esi
//          pop ebx
//          retn 12
//          """
//        )

      }
    }
  }

//  "A simple hashcode test" should "print the correct results" in {
//    val name = System.nanoTime
//    
//    def factorial(n:Int):Int = if (n==0) 1 else n * factorial(n-1)
//    
//    (1 to 12).foreach {
//      input =>
//        val expectedOutput = factorial(input)
//        getProgramOutput(getExecutable(input), false) should equal(expectedOutput.toString)
//    }
//
//  }
}