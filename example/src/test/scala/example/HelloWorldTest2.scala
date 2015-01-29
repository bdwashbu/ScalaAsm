package example

import org.scalatest._
import java.io.DataOutputStream
import java.io.FileOutputStream
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.InputStream
import java.io.BufferedWriter
import java.io.File
import java.io.FileInputStream
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32

object HelloWorld2 extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.General._

  sections += new DataSection (
    Variable("helloWorld", "Hello World!\n\u0000")
  ) {}

  sections += new CodeSection {

    procedure(name = "start",
      call("printHelloWorld"),
      push(ebx),
      call("flushBuffer"),
      retn(()))

    procedure(name = "printHelloWorld",
      push("helloWorld"),
      call("printf"),
      add(esp, byte(4)),
      retn(()))
      
    val numberOfBytesToWrite = *(ebp + byte(-12))
    val numberOfBytesWritten = *(ebp + byte(-8))
    val hFile = *(ebp + byte(-4))
    val lpBuffer = *(ebp + byte(8))
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)
    
    procedure(name = "flushBuffer",
      push(ebp),
      mov(ebp, esp),
      add(esp, byte(-12)),
      push(STD_OUTPUT_HANDLE),
      call("GetStdHandle"),
      mov(hFile, eax),
      push(lpBuffer),
      call("strlen"),
      mov(numberOfBytesToWrite, eax),
      push(byte(0)),
      lea(eax, numberOfBytesWritten),
      push(eax),
      push(numberOfBytesToWrite),
      push(lpBuffer),
      push(hFile),
      call("WriteFile"),
      mov(eax, numberOfBytesWritten),
      leave(()),
      retn(word(4)))

    procedure(name = "strlen",
      mov(eax, *(esp + byte(4))), // pointer to string
      lea(edx, *(eax + byte(3))),
      push(ebp),
      push(edi),
      mov(ebp, dword(0x80808080)),

      label("start"),

      repeat(3, List(
          mov(edi, *(eax)), // read first 4 bytes
          add(eax, byte(4)), // increment pointer
          lea(ecx, *(edi - dword(0x1010101))), // subtract 1 from each byte
          not(edi), // invert all bytes
          and(ecx, edi),
          and(ecx, ebp),
          jnz("test"))),

      mov(edi, *(eax)),
      add(eax, byte(4)),
      lea(ecx, *(edi - dword(0x1010101))),
      not(edi),
      and(ecx, edi),
      and(ecx, ebp),
      jz("start"),

      label("test"),
      test(ecx, dword(0x8080)), // test first 2 bytes
      jnz("end"),
      shr(ecx, byte(0x10)),
      add(eax, byte(2)),
      label("end"),
      shl(cl),
      sbb(eax, edx), // compute length
      pop(edi),
      pop(ebp),
      retn(word(4)))

    builder += align(2)
  }
}

class HelloWorldTest2 extends FlatSpec with ShouldMatchers {
  
  val executableName = "test_HelloWorldTest2.exe"

  "A complex 32-bit Hello world" should "print 'Hello World'" in {
    val name = System.nanoTime
    new File(executableName).delete()
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}

    var beginTime = System.nanoTime()
    val helloWorld = assembler.assemble(HelloWorld2).addIcon("scala.ico")

    val exe = linker.link(helloWorld, 0x3000, false, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec(executableName);
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val output = in.readLine()

    child.waitFor()

    new File(executableName).delete()

    output should equal("Hello World!")
  }
}