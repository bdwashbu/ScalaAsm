package com.scalaAsm.testing

import org.scalatest._
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86._
import com.scalaAsm.x86.Operands.Memory.Indirect

class FibonacciTest extends FlatSpec with ShouldMatchers {

  def getExecutable(input: Int): AsmProgram = {
    new AsmProgram {

      import com.scalaAsm.x86.Instructions.General._
      import com.scalaAsm.x86.Instructions._

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {
        
         builder += Code(
          asm"""
            mov ecx,0
            push $input             // calculate the nth fib
            call fib            // calculate fib (eax)
            add esp, 4          // clean up the stack
        
            push eax
            push test
            call printf
            pop eax
            pop eax
            retn
        """)
        
          procedure(name = "fib",asm"""
            add ecx,1
            push ebp
            mov  ebp,esp
            sub  esp, 4         // space for a local dword [ebp-4]
            mov  eax,[ebp+8]    // get n
        
            cmp  eax,2          // n == 2?
            je   exception2
            cmp  eax,1          // n == 1?
            je   exception2
        
            dec eax
            push eax            // Fib(n-1)
            call fib
            mov [ebp-4], eax    // store first result
        
            dec [esp] // (n-1)
            call fib
            add esp, 4          // clean up stack
        
            add eax, [ebp-4]    // add result and stored first result
        
            jmp Quit
        
        exception2:
            mov eax, 1          // start values: 1, 1
        Quit:
            mov esp, ebp        // restore esp
            pop ebp             // restore ebp
        
            retn                 // return EAX, stack not cleaned up
      """)
      }
    }
  }

  "A simple recursive fibonacci test" should "print the correct results" in {
    val name = System.nanoTime
    
    def fibonacci( n : Int) : Int = n match {
       case 0 | 1 => n
       case _ => fibonacci( n-1 ) + fibonacci( n-2 )
    }
    
    (1 to 10).foreach {
      input =>
        val expectedOutput = fibonacci(input)
        getProgramOutput(getExecutable(input), false) should equal(expectedOutput.toString)
    }

  }
}