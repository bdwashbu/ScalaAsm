Scala x86
========
#### A x86 Assembler/Linker written in Scala

* Have you ever wanted to design your own programming language?

* Have you ever wondered how executable files work on windows?

* You've wanted to learn assembly/x86?

Scala x86 is a collaboration of all these concepts.  Scala x86 can assemble x86 code as well as link this into a portable executable (PE) file for execution on windows 32 or 64-bit platforms.  


#### Implementing x86

Many shareholders, mainly Intel, have developed x86 since 1978 and it has grown into a very large instruction set.  There are hundreds of instructions, and each instruction could have upwards of 20 different types of inputs!  To put this into programming terms - each instruction could be overloaded many, many times.  This means there are possibly 10+ thousand instructions needing to be implemented if you want to be thorough.  The Intel x86 spec is 3300 pages, after all.

For the implementers, Scala x86 strives to make the process of defining instructions as easy and simple as possible.  It was designed for this.  It resembles the Intel specification at all times.  When you have thousands of definitions like this, strong type safety and consise notation help reduce the chance of errors.  Scala x86 provides this, which is useful because there are still thousands of instructions that have yet to be implemented.

For the users, Scala x86 offers compile-time safety.  If bad operand types are passed into an instruction the Scala compiler will see this and flag an error, using type classes. 

##### Instructions

Heres an example of some PUSH instructions for 16-bit register, 8-bit immediate value, and 16-bit immediate value operands.

```scala
implicit object push1 extends PUSH_1[r16] {
  def operands = O()
  def opcode = OpcodePlusRd(0x50)
}
  
implicit object push2 extends PUSH_1[imm8] {
  def operands = I()
  val opcode: Opcodes = 0x6A
}
  
implicit object push3 extends PUSH_1[imm16] {
  def operands = I()
  val opcode: Opcodes = 0x68
}
```

[See more instructions](/x86/src/main/scala/com/scalaAsm/x86/Instructions "More instructions")

Here we see PUSH definitions straight from the Intel x86 specification, and we see that the definitions look similiar. The "Op/En" field is very important here. As seen in the code above, Op/En along with the opcode gives us enough information to completely implement the instruction!

![Alt text](/example/push.png "PUSH examples")

The "Op/En" field is an abbreviation for the type of expected operands e.g "I" means "Immediate".

![Alt text](/example/pushOpEncoding.png "PUSH examples")


Assuming those are the only versions of PUSH available, if you tried calling:

```scala
push(rcx)
```

It would be a compile time error because there is no PUSH implementation defined that takes a single 64-bit register.

### Using Scala x86

The hope is that Scala x86 can be used to implement a turing-complete programming language, but this is still an area of research.

We do know Scala x86 can be used to implement low-level assembly code. This assembly currently supports many useful  features such as procedures, loops, labels, and variables.  Some of these, like variables and loops, are implemented using first-class scala constructs.

Heres a windows console version of "Hello world!":

```scala
object HelloWorld2 extends AsmProgram {
  
  dataSections += new DataSection {
    builder += Variable("helloWorld", "Hello World!\r\n\0")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\0")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = imm8(-11)
    val STD_INPUT_HANDLE = imm8(-10)

    procedure(name = "start",
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(ebx, eax),
      push(imm8(0)),
      push(imm8(0)),
      push(imm8(0x0E)),
      push("helloWorld"),
      push(eax),
      call("WriteFile"),
      push(imm8(0)),
      push(imm8(0)),
      push(imm8(0x1D)),
      push("pressAnyKey"),
      push(ebx),
      call("WriteFile"),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      mov(eax, imm32(0)),
      retn
    )
  }
}
```

Heres code to output the executable:

```scala
val asm = HelloWorld.assemble
val exe = ExeGenerator.link(asm, 0x2000, "kernel32.dll", "msvcrt.dll")

val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
outputStream.write(exe.get)
outputStream.close
```

References:
========

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
