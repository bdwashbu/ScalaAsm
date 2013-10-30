ScalaAsm
========
#### x86 Assembler written in Scala

Have you ever wondered how executable files work, or if a high-level language like Scala is suitable for x86 machine code generation?

Rest assured, Scala is great for this task and ScalaAsm is the answer for you!

ScalaAsm is an academic exercise that explores whether things like typeclasses, extractors, and pattern matching are beneficial to machine code generation.  The answer so far is 'Most definitely!'.

Currently, the library is designed to assemble an executable from the ground up, using a built-in DSL (Domain Specific Language).
The ultimate goal is to use ScalaAsm to implement a BASIC programming language!

Heres an example assembly program:

```scala
object HelloWorld extends AsmProgram {

  val data = new Data {
    val pressAnyKey = "Press any key to continue ...\0"
    val newline = "\r\n\0"
    val helloWorld = "Hello World!\n\0"
  }
  
  val code = new Code {

    proc("start") {
      call("printHelloWorld")
      push("pressAnyKey")
      call("flushBuffer")
      call("waitForKeypress")
      push("newline")
      call("flushBuffer")
      push(imm8(0))
      call("ExitProcess")
    }

    proc("printHelloWorld") {
      push("helloWorld")
      call("printf")
      add(esp, imm8(4))
      retn
    }
     
    proc("flushBuffer") {
      
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

    proc("waitForKeypress") {
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

    proc("sub_4010A0") {
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

    proc("ExitProcess") {
      jmp("ExitProcess")
    }

    proc("GetStdHandle") {
      jmp("GetStdHandle")
    }

    proc("WriteFile") {
      jmp("WriteFile")
    }

    proc("FlushConsoleInputBuffer") {
      jmp("FlushConsoleInputBuffer")
    }

    proc("Sleep") {
      jmp("Sleep")
    }
  }
}
```

Heres code to output the executable:

```scala
val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
val assembled = HelloWorld.assemble
val exe = ExeGenerator.compile(assembled)
outputStream.write(exe.get)
outputStream.close
```

ScalaAsm currently supports many useful Assembly features such as procedures, labels, and variables.

Additionally, many errors can be caught at compile time:

If an instruction is not supported or not yet implemented it will throw an error based on the parameters passed in e.g

```scala
lea(rcx, *(rdi - imm32(0x1010101))) // compile-time error (64-bit is not yet supported)
```

Typeclasses are used to implement the many forms of x86 instructions.  A primary goal was to have the code resemble the intel x86 reference manual.

x86 reference can be found here: http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html

How to run:
========

1. Launch sbt
2. type 'project example'
3. type 'run'
4. Observe 'test.exe' get generated
5. Execute 'test.exe'

Output:
```
Hello World!
Press any key to continue ...
```
