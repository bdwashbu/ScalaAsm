Scala x86 ![Alt text](/example/smooth-spiral.png) ![Alt text](/example/exe-icon1.gif)
========

Welcome to the best Scala source for all things [x86-64](http://en.wikipedia.org/wiki/X86), [Portable Executable](http://en.wikipedia.org/wiki/Portable_Executable) (.exe/dll), and [COFF](http://en.wikipedia.org/wiki/COFF) (.o/obj)!

Scala x86 is very low level code written in a very high level language.  Its a back-end for a compiler.  A number of projects comprise of scala x86 and these assist in assembling and linking.

ScalaAsm is assembly which uses Scala x86 and macros.  It can assemble code into a object file and then link it into an executable or dll for windows 32 or 64-bit platforms.  What makes ScalaAsm special is the amount of compile time safety within the API. ![Alt text](/example/exeicon.png)

#### Implementing x86-64

Intel and others have worked on x86 since 1978 and it has grown into a very large instruction set - the documentation is over 3300 pages.  There are over 700 instructions and each one could have dozens of versions with different types of inputs.  The instruction MOV has over 30 different versions, for instance.  When you account for each version there are more than 1300 unique instruction signatures. 

At the end of the day, there are far too many x86 instructions to maintain the code behind them manually.  It would be difficult and time consuming to make the most trivial changes.  To get around this, Scala x86 uses auto-generated instruction code definitions which comes from a very helpful XML file.  The XML can be found in references below.

All the auto-generated instruction definitions are members of a type class.  The end result is great compile time safety and readable code if you want to look at the definitions. ![Alt text](/example/dllicon.png)

##### Instructions

Heres an example of some PUSH instructions for 64-bit register, 16-bit register, 8-bit immediate value, and 16-bit immediate value operands.

```scala
implicit object push1 extends _1[r64] {
  def opcode = 0x50 + rd
}

implicit object push2 extends _1[r16] {
  def opcode = 0x50 + rw
}
  
implicit object push3 extends _1[imm8] {
  def opcode = 0x6A
}
  
implicit object push4 extends _1[imm16] {
  def opcode = 0x68
}
```

[See more instructions](https://github.com/bdwashbu/scala-x86-inst/tree/master/src/main/scala/com/scalaAsm/x86/Instructions "More instructions")

Here we see PUSH definitions straight from the Intel x86 specification, and we see that the definitions look similiar. As seen in the code above, sometimes just the opcode is enough information to completely define the instruction!

![Alt text](/example/push.png "PUSH examples")

Assuming those are the only versions of PUSH available, if you tried calling:

```scala
push(ecx)
```

It would be a compile time error because there is no PUSH implementation defined that takes a single 32-bit register. ![Alt text](/example/exeicon.png)

### ScalaAsm

Scala x86 can be used to implement low-level assembly code. ScalaAsm currently supports many useful features such as procedures, loops, labels, and variables.  Some of these, like variables, are implemented using scala code.

Heres a short 32-bit windows console version of "Hello world!", without using the C standard library:

```scala
object HelloWorld extends AsmProgram {

  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.asm._
  
  sections += new DataSection(
    Variable("helloWorld", "Hello World!\r\n")
  ) {}

  sections += new CodeSection {
    builder += Code(asm"""
      mov     ebp, esp
      sub     esp, 4
      push    -11
      call    GetStdHandle
      mov     ebx, eax    
      push    0
      lea     eax, [ebp-4]
      push    eax
      push    12
      push    helloWorld
      push    ebx
      call    WriteFile
      push    0
      call    ExitProcess
      """
    )
  }
}
```

Heres code to build and output the executable:

```scala
val obj = assembler.assemble(HelloWorld)
val exe = linker.link(obj, 0x2000, is64Bit=false, "kernel32.dll", "msvcrt.dll")

val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
outputStream.write(exe.get)
outputStream.close
```

#### References:

A huge shoutout to Jeremy Gordon and Wayne Radburn for GoAsm, the best 64-bit windows assembler!  
http://www.godevtool.com/

The best x86 reference site!  The master x86 XML comes from here.  
http://ref.x86asm.net/

#### Specs:

Intel® 64 and IA-32 Architectures Software Developer Manuals:   http://www.intel.com/content/www/us/en/processors/architectures-software-developer-manuals.html

Microsoft PE format spec:  
http://msdn.microsoft.com/en-us/library/windows/desktop/ms680547(v=vs.85).aspx

#### Tools

[PE View](http://wjradburn.com/software/)  
PE Viewer  
[Hopper dissembler](http://www.hopperapp.com/)  
IDA  

#### Informational:

The Portable Executable format from top to bottom:  
http://www.csn.ul.ie/~caolan/publink/winresdump/winresdump/doc/pefile2.html

A Tour of the Win32 Portable Executable File Format:  
http://msdn.microsoft.com/en-us/library/ms809762.aspx

X86 Opcode and Instruction Reference:  
http://ref.x86asm.net/index.html

Encoding x86 instructions:  
http://www.c-jump.com/CIS77/CPU/x86/lecture.html#X77_0240_prefix

#### Interesting Articles:

Redundancy within x86:  
http://www.strchr.com/machine_code_redundancy

### How to run:

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

### How to build:

1. Launch sbt from base directory
2. Run the 'eclipse with-source=true' using sbtEclipse to generate eclipse files
3. Refresh/Clean projects in eclipse

### Misc:

Adding an icon to an executable:

Before: ![Alt text](/example/beforeicon.png)
```
assembler.assemble(HelloWorld).addIcon("scala.ico")
```
After: ![Alt text](/example/aftericon.png)
