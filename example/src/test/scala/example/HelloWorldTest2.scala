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
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._

object HelloWorld2 extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.General._

  sections += new DataSection (
    Variable("helloWorld", "Hello World!\n\u0000")
  ) {}

  sections += new CodeSection {

    procedure(name = "start",
      asm"call printHelloWorld",
      asm"push ebx",
      asm"call flushBuffer",
      asm"retn")

    procedure(name = "printHelloWorld",
      asm"push helloWorld",
      asm"call printf",
      asm"add esp, 4",
      asm"retn")
      
    val numberOfBytesToWrite = ebp + byte(-12)
    val numberOfBytesWritten = ebp + byte(-8)
    val hFile = ebp + byte(-4)
    //val hFileTest = asm"[ebp - 4]"
    val lpBuffer = ebp + byte(8)
    val STD_OUTPUT_HANDLE = "-14"
    val STD_INPUT_HANDLE = byte(-10)
    
    
    
    procedure(name = "flushBuffer",
      asm"push ebp",
      asm"mov ebp, esp",
      asm"add esp, -12",
      //add(esp, byte(-12)),
      //asm"push $STD_OUTPUT_HANDLE",
      asm"call GetStdHandle",
      MOV(hFile, eax),
      PUSH(lpBuffer),
      asm"call strlen",
      MOV(numberOfBytesToWrite, eax),
      PUSH(byte(0)),
      LEA(eax, numberOfBytesWritten),
      asm"push eax",
      PUSH(numberOfBytesToWrite),
      PUSH(lpBuffer),
      PUSH(hFile),
      asm"call WriteFile",
      MOV(eax, numberOfBytesWritten),
      asm"leave",
      RETN(word(4)))

    procedure(name = "strlen",
      MOV(eax, esp + byte(4)), // pointer to string
      LEA(edx, eax + byte(3)),
      asm"push ebp",
      asm"push edi",
      MOV(ebp, dword(0x80808080)),

      asm"start:",

      repeat(3, List(
          MOV(edi, *(eax)), // read first 4 bytes
          asm"ADD eax, 4", // increment pointer
          LEA(ecx, edi - dword(0x1010101)), // subtract 1 from each byte
          asm"not edi", // invert all bytes
          asm"and ecx, edi",
          asm"and ecx, ebp",
          asm"jnz test")),

      MOV(edi, *(eax)),
      asm"add eax, 4",
      LEA(ecx, edi - dword(0x1010101)),
      asm"not edi",
      asm"and ecx, edi",
      asm"and ecx, ebp",
      asm"jz start",

      asm"test:",
      TEST(ecx, dword(0x8080)), // test first 2 bytes
      asm"jnz end",
      asm"shr ecx, byte 16",
      asm"add eax, 2",
      asm"end:",
      asm"shl cl",
      asm"sbb eax, edx", // compute length
      asm"pop edi",
      asm"pop ebp",
      RETN(word(4)))

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