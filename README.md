ScalaAsm
========

Have you ever wondered how executable files work, or if a high-language like Scala is suitable for low-level x86 machine code generation?

Rest assured, Scala is great for this task and ScalaAsm is the answer for you!

ScalaAsm is designed to assemble an executable from scratch, using nothing more than a built-in DSL (Domain specific language)

Heres an example:

```scala
object HelloWorld extends AsmProgram {

  val data = new Data {
    "pressAnyKey" db "Press any key to continue ..." + 0x00.toChar
    "newline" db hex2Bytes("0D 0A 00").map(_.toChar).mkString
    align(0x4, 0x00)
    "helloWorld" db "Hello World!\n" + 0x00.toChar
    align(0x4, 0x00)
  }
  
  val code = new Code {

    new Proc("start") {
      call("sub_401025")
      push("pressAnyKey")
      call("sub_401034")
      call("sub_401070")
      push("newline")
      call("sub_401034")
      push(imm8(0))
      call("ExitProcess")
    }

    new Proc("sub_401025") {
      push("helloWorld")
      call("printf")
      add(esp, imm8(4))
      retn
    }
     
    new Proc("sub_401034") {
      
      val numberOfBytesToWrite = imm8(-12)
      val numberOfBytesWritten = imm8(-8)
      val hFile = imm8(-4)
      val lpBuffer = imm8(8)
      
      push(ebp)
      mov(ebp, esp)
      add(esp, imm8(-12))
      push(imm8(0xF5))
      call("GetStdHandle")
      mov(*(ebp + hFile), eax)
      push(*(ebp + lpBuffer))
      call("sub_4010A0")
      mov(*(ebp + numberOfBytesToWrite), eax)
      push(imm8(0))
      lea(eax, *(ebp + numberOfBytesWritten))
      push(eax)
      push(*(ebp + numberOfBytesToWrite))
      push(*(ebp + lpBuffer))
      push(*(ebp + hFile))
      call("WriteFile")
      mov(eax, *(ebp + numberOfBytesWritten))
      leave
      retn(imm16(4))
    }

    align(0x10)

    new Proc("sub_401070") {
      push(imm8(-10)) //nStdHandle
      call("GetStdHandle")
      push(eax)
      call("FlushConsoleInputBuffer")
      push(imm8(1))
      call("Sleep")
      call("_kbhit")
      test(eax, eax)
      jz(imm8(-17))
      call("_getch")
      retn
    }

    align(0x10)

    new Proc("sub_4010A0") {
      mov(eax, *(esp + imm8(4)))
      lea(edx, *(eax + imm8(3)))
      push(ebp)
      push(edi)
      mov(imm32(0x80808080))
      label("start")
      mov(edi, *(eax))
      add(eax, imm8(4))
      lea(ecx, *(edi - imm32(0x1010101)))
      not(edi)
      and(ecx, edi)
      and(ecx, ebp)
      jnz("test")
      mov(edi, *(eax))
      add(eax, imm8(4))
      lea(ecx, *(edi - imm32(0x1010101)))
      not(edi)
      and(ecx, edi)
      and(ecx, ebp)
      jnz("test")
      mov(edi, *(eax))
      add(eax, imm8(4))
      lea(ecx, *(edi - imm32(0x1010101)))
      not(edi)
      and(ecx, edi)
      and(ecx, ebp)
      jnz("test")
      mov(edi, *(eax))
      add(eax, imm8(4))
      lea(ecx, *(edi - imm32(0x1010101)))
      not(edi)
      and(ecx, edi)
      and(ecx, ebp)
      jz("start")
      label("test")
      test(ecx, imm32(0x8080))
      jnz("end")
      shr(ecx, imm8(0x10))
      add(eax, imm8(2))
      label("end")
      shl(cl)
      sbb(eax, edx)
      pop(edi)
      pop(ebp)
      retn(imm16(4))
    }

    align(2)

    new Proc("ExitProcess") {
      jmp("ExitProcess")
    }

    new Proc("GetStdHandle") {
      jmp("GetStdHandle")
    }

    new Proc("WriteFile") {
      jmp("WriteFile")
    }

    new Proc("FlushConsoleInputBuffer") {
      jmp("FlushConsoleInputBuffer")
    }

    new Proc("Sleep") {
      jmp("Sleep")
    }
  }
}
```

ScalaAsm currently supports many useful Assembly features such as procedures, labels, and variables.

Additionally, many errors can be caught at compile time:

If an instruction is not supported or not yet implemented it will throw an error based on the parameters passed in e.g

```scala
lea(rcx, *(rdi - imm32(0x1010101))) // compile-time error (64-bit is not yet supported)
```

Typeclasses are used to implement the many forms of instructions, with no runtime overhead.

x86 reference can be found here: http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html
