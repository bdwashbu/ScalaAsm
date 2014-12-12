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
import com.scalaAsm.x86.Operands.addr

object HelloWorld64 extends AsmProgram[x86_64] {
  
  import com.scalaAsm.x86.Instructions.Standard._
  
  sections += new DataSection (
    Variable("helloWorld", "Hello World!\r\n\u0000")
  ) {}

  sections += new CodeSection {
    
    val STD_OUTPUT_HANDLE = qword(-11)
    val STD_INPUT_HANDLE = qword(-10)

    procedure(name = "start",
      push(rsp),
      push(*(rsp)),
      and(spl, byte(0xF0)),
      mov(rdx, dword(0xE)),
      lea(rcx, addr("helloWorld")),
      sub(rsp, byte(0x20)),
      invoke("printf"), // needs work
      lea(rsp, *(rsp+byte(0x28))),
      pop(rsp),
      xor(rax, rax),
      retn(())
    )
  }
}

class HelloWorldTest64 extends FlatSpec with ShouldMatchers {

  "A 64-bit Hello world" should "print 'Hello World'" in {
    val name = System.nanoTime
    val outputStream = new DataOutputStream(new FileOutputStream("test.exe"));
    val assembler = new Assembler {}
    val linker = new Linker {}

    var beginTime = System.nanoTime()
    val helloWorld = assembler.assemble(HelloWorld64).addIcon("scala.ico")

    val exe = linker.link(helloWorld, 0x3000, true, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec("test.exe");
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val output = in.readLine()

    child.waitFor()

    new File("test.exe").delete()

    output should equal("Hello World!")
  }
}