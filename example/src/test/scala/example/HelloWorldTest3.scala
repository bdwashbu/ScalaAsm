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
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86._
import com.scalaAsm.asm._

object HelloWorld3 extends AsmProgram[x86_32] {
  
  import com.scalaAsm.x86.Instructions.General._

  sections += new DataSection (
    Variable("pressAnyKey", "Press any key to continue ...\n\0")
  ) {}

  sections += new CodeSection {

    procedure(name = "start",
      asm"push pressAnyKey",
      asm"call flushBuffer",
      PUSH(byte(0)),
      asm"call ExitProcess")
      
    val numberOfBytesToWrite = ebp + byte(-12)
    val numberOfBytesWritten = ebp + byte(-8)
    val hFile = ebp + byte(-4)
    val lpBuffer = ebp + byte(8)
    val STD_OUTPUT_HANDLE = byte(-11)
    val STD_INPUT_HANDLE = byte(-10)
    
    procedure(name = "flushBuffer",
      PUSH(ebp),
      MOV(ebp, esp),
      ADD(esp, byte(-12)),
      PUSH(STD_OUTPUT_HANDLE),
      asm"call GetStdHandle",
      MOV(hFile, eax),
      PUSH(lpBuffer),
      asm"call strlen",
      MOV(numberOfBytesToWrite, eax),
      PUSH(byte(0)),
      LEA(eax, numberOfBytesWritten),
      PUSH(eax),
      PUSH(numberOfBytesToWrite),
      PUSH(lpBuffer),
      PUSH(hFile),
      asm"call WriteFile",
      MOV(eax, numberOfBytesWritten),
      LEAVE(()),
      RETN(word(4)))

    procedure(name = "strlen",
      MOV(eax, esp + byte(4)), // pointer to string
      LEA(edx, eax + byte(3)),
      PUSH(ebp),
      PUSH(edi),
      MOV(ebp, dword(0x80808080)),

      asm"start:",

      repeat(3, List(
          MOV(edi, *(eax)), // read first 4 bytes
          ADD(eax, byte(4)), // increment pointer
          LEA(ecx, edi - dword(0x1010101)), // subtract 1 from each byte
          NOT(edi), // invert all bytes
          AND(ecx, edi),
          AND(ecx, ebp),
          asm"jnz test")),

      MOV(edi, *(eax)),
      ADD(eax, byte(4)),
      LEA(ecx, edi - dword(0x1010101)),
      NOT(edi),
      AND(ecx, edi),
      AND(ecx, ebp),
      asm"jz start",

      asm"test:",
      TEST(ecx, dword(0x8080)), // test first 2 bytes
      asm"jnz end",
      SHR(ecx, byte(0x10)),
      ADD(eax, byte(2)),
      asm"end:",
      SHL(cl),
      SBB(eax, edx), // compute length
      POP(edi),
      POP(ebp),
      RETN(word(4)))

    builder += align(2)
  }
}

class HelloWorldTest3 extends FlatSpec with ShouldMatchers {
  
  val executableName = "test_HelloWorldTest3.exe"

  "A test of flushbuffer" should "print 'Press any key to continue...'" in {
    val name = System.nanoTime
    new File(executableName).delete()
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}

    var beginTime = System.nanoTime()
    val helloWorld = assembler.assemble(HelloWorld3).addIcon("scala.ico")

    val exe = linker.link(helloWorld, 0x3000, false, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec(executableName);
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val output = in.readLine()
    
    println(output)

    child.waitFor()

    new File(executableName).delete()

    output should equal("Press any key to continue ...")
  }
}