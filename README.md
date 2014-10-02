Scala x86
========
#### An x86 Assembler/Linker written in Scala

* Have you ever wanted to design your own programming language?

* Have you ever wondered how executable files work on windows?

* Maybe you've wanted to learn assembly/x86?

Scala x86 is a collaboration of all these concepts.  Scala x86 can assemble x86 code and link this into a portable executable (PE) file for execution on windows 32 and 64-bit platforms.  


#### Implementing x86

Intel and others have worked on x86 since 1978 and it has grown into a very large instruction set.  There are hundreds of instructions and each one could have upwards of 20 different types of inputs!  To put this into programming terms - each instruction could be overloaded many, many times.  This means there are possibly 10+ thousand instructions needing to be implemented if you want to be thorough.  The Intel x86 spec is 3300 pages, after all.

For the implementers, Scala x86 strives to make the process of defining instructions as easy and simple as possible.  It was designed for this.  It resembles the Intel specification at all times.  When you have thousands of definitions like this, strong type safety and consise notation help reduce the chance of errors.  Scala x86 provides this, which is useful because there are still thousands of instructions that have yet to be implemented.

For the users, Scala x86 offers compile-time safety.  If bad operand types are passed into an instruction the Scala compiler will see this and flag an error, using type classes. 

##### Instructions

Heres an example of some PUSH instructions for 16-bit register, 8-bit immediate value, and 16-bit immediate value operands.

```scala
implicit object push1 extends PUSH_1[O, r16] {
  def opcode = OpcodePlusRd(0x50)
}
  
implicit object push2 extends PUSH_1[I, imm8] {
  val opcode: Opcodes = 0x6A
}
  
implicit object push3 extends PUSH_1[I, imm16] {
  val opcode: Opcodes = 0x68
}
```

[See more instructions](/x86/src/main/scala/com/scalaAsm/x86/Instructions/Standard "More instructions")

Here we see PUSH definitions straight from the Intel x86 specification, and we see that the definitions look similiar. The "Op/En" field is very important here. As seen in the code above, Op/En along with the opcode gives us enough information to completely implement the instruction (at a high level)!

![Alt text](/example/push.png "PUSH examples")

The "Op/En" field is an abbreviation for the type of expected operands e.g "I" means either imm8, imm16, or imm32.

![Alt text](/example/pushOpEncoding.png "PUSH examples")


Assuming those are the only versions of PUSH available, if you tried calling:

```scala
push(rcx)
```

It would be a compile time error because there is no PUSH implementation defined that takes a single 64-bit register.

### Using Scala x86

The hope is that Scala x86 can be used to implement a turing-complete programming language, but this is still an area of research.

We do know Scala x86 can be used to implement low-level assembly code. This assembly currently supports many useful  features such as procedures, loops, labels, and variables.  Some of these, like variables, are implemented using first-class scala constructs.

Heres a windows console version of "Hello world!":

```scala
object HelloWorld2 extends AsmProgram {
  
  dataSections += new DataSection {
    builder += Variable("helloWorld", "Hello World!\r\n\0")
    builder += Variable("pressAnyKey", "Press any key to continue ...\r\n\0")
  }

  codeSections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)

    procedure(name = "start",
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(ebx, eax),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x0E)),
      push("helloWorld"),
      push(eax),
      call("WriteFile"),
      push(byte(0)),
      push(byte(0)),
      push(byte(0x1D)),
      push("pressAnyKey"),
      push(ebx),
      call("WriteFile"),
      push(STD_INPUT_HANDLE),
      call("GetStdHandle"),
      push(eax),
      call("FlushConsoleInputBuffer"),
      call("_getch"),
      mov(eax, dword(0)),
      retn
    )
  }
}
```

Heres code to output the executable:

```scala
val asm = HelloWorld.assemble
val exe = asm.link(0x2000, "kernel32.dll", "msvcrt.dll")

val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
outputStream.write(exe.get)
outputStream.close
```

References:
========

A huge shoutout to Jeremy Gordon for GoAsm, the best 64-bit windows assembler, which I am quite inspired by
http://www.godevtool.com/

Intel® 64 and IA-32 Architectures Software Developer Manuals: http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html

Microsoft PE format spec:
http://msdn.microsoft.com/en-us/library/windows/desktop/ms680547(v=vs.85).aspx

The Portable Executable format from top to bottom:
http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/pefile2.html

A Tour of the Win32 Portable Executable File Format:
http://msdn.microsoft.com/en-us/library/ms809762.aspx

X86 Opcode and Instruction Reference:
http://ref.x86asm.net/index.html

Encoding x86 instructions:
http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0240_prefix

How to run:
========

1. Launch sbt
2. type 'project example'
3. type 'run'
4. Observe 'test.exe' get generated
5. Execute 'test.exe'

How to build:
========

1. Set any dependencies in the top level build.sbt file
2. Launch sbt
3. Run the 'eclipse' command to rebuild all .classpath and .project files
4. 

Output:
```
Hello World!
Press any key to continue ...
```
