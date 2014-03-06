Scala x86
========
#### State-of-the-art x86 Assembler written in Scala

* Have you ever wanted to design your own programming language?

* Have you ever wondered how executable files work?

* Perhaps you've wanted to learn x86/assembly?

Scala x86 can help teach  all these things.  The library performs two main functions: emit x86 machine code + assemble this into a portable executable file for execution on windows.  


#### Implementing x86

Lets face it, you have to be a little crazy to go this low-level.  x86 has been developed over decades and it has grown into a very large instruction set.  There are over 1000 base instructions, and each instruction could have upwards of 20 different types of inputs!  To put this into programming terms - each instruction could be overloaded many, many times.  This means there are possibly 10's of thousands of instructions needing to be implemented if you want to be thorough.  

That being said, Scala x86 strives to make the process of defining instructions as easy as possible.  It was designed for this.  It also resembles the intel specification at all times, especially when it comes to terminology.  When you have thousands of definitions like this, strong type safety and consise notation help reduce the chance of errors.  Scala x86 provides this.

Heres an example of some one-operand PUSH implementations for a 16-bit register, 8-bit immediate value, and 16-bit immediate value.

```scala
implicit object push8 extends PUSH_1[r16] {
  def operands = O(x)
  def opcode = OpcodePlusRd(0x50, x)
}
  
implicit object push4 extends PUSH_1[imm8] {
  def operands = I[imm8](x)
  val opcode: Opcodes = 0x6A
}
  
implicit object push5 extends PUSH_1[imm16] {
  def operands = I[imm16](x)
  val opcode: Opcodes = 0x68
}
```

Each instruction takes only 4 lines of code!

### Using Scala x86

You can use Scala x86 to power any sort of programming language you'd like.  

Heres an example some low-level assembly:

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
      
      val numberOfBytesToWrite = *(ebp + imm8(-12))
      val numberOfBytesWritten = *(ebp + imm8(-8))
      val hFile = *(ebp + imm8(-4))
      val lpBuffer = *(ebp + imm8(8))
      val STD_OUTPUT_HANDLE = imm8(-11)
      
      push(ebp)
      mov(ebp, esp)
      add(esp, imm8(-12))
      push(STD_OUTPUT_HANDLE)
      call("GetStdHandle")
      mov(hFile, eax)
      push(lpBuffer)
      call("strlen")
      mov(numberOfBytesToWrite, eax)
      push(imm8(0))
      lea(eax, numberOfBytesWritten)
      push(eax)
      push(numberOfBytesToWrite)
      push(lpBuffer)
      push(hFile)
      call("WriteFile")
      mov(eax, numberOfBytesWritten)
      leave
      retn(imm16(4))
    }

    align(0x10)

    proc("waitForKeypress") {
      
      val STD_INPUT_HANDLE = imm8(-10)
      
      push(STD_INPUT_HANDLE)
      call("GetStdHandle")
      push(eax)
      call("FlushConsoleInputBuffer")
      push(imm8(1))
      call("Sleep")
      call("_kbhit")
      test(eax, eax) // eax is 0 if a key has not been pressed
      jz(imm8(-17)) // if a key has not been pressed, loop around again
      call("_getch")
      retn
    }

    align(0x10)

    proc("strlen") {
      
      mov(eax, *(esp + imm8(4))) // pointer to string
      lea(edx, *(eax + imm8(3)))
      push(ebp)
      push(edi)
      mov(ebp, imm32(0x80808080))
      
      label("start")
      
      for (i <- 0 until 3) {
	      mov(edi, *(eax)) // read first 4 bytes
	      add(eax, imm8(4)) // increment pointer
	      lea(ecx, *(edi - imm32(0x1010101))) // subtract 1 from each byte
	      not(edi) // invert all bytes
	      and(ecx, edi)
	      and(ecx, ebp)
	      jnz("test")
      }

      mov(edi, *(eax))
      add(eax, imm8(4))
      lea(ecx, *(edi - imm32(0x1010101)))
      not(edi)
      and(ecx, edi)
      and(ecx, ebp)
      jz("start")
      
      label("test")
      test(ecx, imm32(0x8080)) // test first 2 bytes
      jnz("end")
      shr(ecx, imm8(0x10))
      add(eax, imm8(2))
      label("end")
      shl(cl)
      sbb(eax, edx) // compute length
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

ScalaAsm currently supports many useful Assembly features such as procedures, loops, labels, and variables.  Some of these, like variables and loops, are implemented using first-class scala constructs.

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
