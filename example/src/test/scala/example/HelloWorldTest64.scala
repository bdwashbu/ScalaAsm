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
import com.scalaAsm.asm.x86_64
import com.scalaAsm.asm._

object HelloWorld64 extends AsmProgram[x86_64] {
  
  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.x86.Operands._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\u0000")
  ) {}

  sections += new CodeSection {

    procedure(name = "start",
      asm"push rsp",
      PUSH(*(rsp)),
      AND(spl, byte(0xF0)),
      MOV(rdx, dword(0xE)),
      LEA(rcx, addr("helloWorld")),
      SUB(rsp, byte(0x20)),
      asm"invoke printf", // needs work
      LEA(rsp, rsp + byte(0x28)),
      asm"pop rsp",
      asm"xor rax, rax",
      RETN(())
    )
  }
}

class HelloWorldTest64 extends FlatSpec with ShouldMatchers {
  
  val executableName = "test_HelloWorldTest64.exe"

  "A 64-bit Hello world" should "print 'Hello World'" in {
    val name = System.nanoTime
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}

    var beginTime = System.nanoTime()
    val helloWorld = assembler.assemble(HelloWorld64).addIcon("scala.ico")

    val exe = linker.link(helloWorld, 0x3000, true, "kernel32.dll", "msvcrt.dll")

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