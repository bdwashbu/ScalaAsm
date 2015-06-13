
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import com.scalaAsm.asm.x86_32
import com.scalaAsm.portableExe.PortableExecutable
import org.scalatest._

import org.scalatest.concurrent.TimeLimitedTests
import org.scalatest.time.SpanSugar._

package object example {
  
  def getProgramOutput(program: AsmProgram, is64Bit: Boolean): String = {
    
    var output = ""

    val executableName = s"test${sync.getTestID}.exe"

    val name = System.nanoTime
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}  

    var beginTime = System.nanoTime()
    val obj = assembler.assemble(program).addIcon("scala.ico")

    val exe = linker.link(obj, 0x3000, is64Bit, false, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec(executableName);
    
    Thread.sleep(150)
    try {
      child.exitValue()
    } catch {
      case exception: IllegalThreadStateException => throw new Exception("Test took too long!")
    }
    
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    output = in.readLine()

    child.waitFor()

    new File(executableName).delete()
    output
  }
  
}