package com.scalaAsm

import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86._
import com.scalaAsm.x86.Instructions._

class PowTest {//}extends FlatSpec with ShouldMatchers {

  val executableName = "test_PowTest.exe"
  
  def getExecutable(input: Int): AsmProgram = {
    new AsmProgram {

      import com.scalaAsm.x86.Instructions.General._
      import com.scalaAsm.x86.Instructions.x87._
      import com.scalaAsm.x86.Instructions._

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {
        
        val a = ebp + qword(8)
        val b = ebp + qword(16)
        val result = ebp + dword(24)
        val ctrlWord = ebp - word(2)
        val tmp = ebp - dword(6)       
        
//        builder += Code(asm"""
//          push ebp
//          mov ebp, esp
//          sub esp, 6
//          push ebx
//      
//          fstcw $ctrlWord
//          or $ctrlWord, 0xC00
//          fldcw $ctrlWord
//      
//          fld b
//          fld a
//          fyl2x
//      
//          fist $tmp
//      
//          fild $tmp
//          fsub
//          f2xm1
//          fld1
//          fadd
//          fild $tmp
//          fxch
//          fscale
//      
//          mov ebx, $result
//          fst [ebx]
//      
//          pop ebx
//          mov esp, ebp
//          pop ebp
//          
//          push ebx
//          push test
//          call printf
//          pop ebx
//          pop ebx
//          
//          retn"""
//        )

      }
    }
  }

//  "A simple pow test" should "print the correct results" in {
//    val name = System.nanoTime
//    
//    //(1 to 12).foreach {
//    //  input =>
//    //    val expectedOutput = factorial(input)
//    //    println(expectedOutput)
//        getProgramOutput(getExecutable(6), false) should equal(expectedOutput.toString)
//    //}
//
//  }
}