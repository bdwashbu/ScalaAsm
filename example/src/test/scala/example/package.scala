
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io.BufferedReader
import java.io.InputStreamReader
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import com.scalaAsm.asm.x86_32

package object example {

  var iteration = 0
  
  def getProgramOutput(program: AsmProgram, is64Bit: Boolean): String = {
    
    var executableName = ""
    synchronized {
      iteration += 1
      executableName = s"test$iteration.exe"
    }

    val name = System.nanoTime
    val outputStream = new DataOutputStream(new FileOutputStream(executableName));
    val assembler = new Assembler {}
    val linker = new Linker {}  

    var beginTime = System.nanoTime()
    val obj = assembler.assemble(program).addIcon("scala.ico")

    val exe = linker.link(obj, 0x3000, is64Bit, "kernel32.dll", "msvcrt.dll")

    outputStream.write(exe.get)
    println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
    outputStream.close

    val child = Runtime.getRuntime().exec(executableName);
    val in = new BufferedReader(
      new InputStreamReader(child.getInputStream()));

    val output = in.readLine()

    child.waitFor()

    new File(executableName).delete()

    output
  }
  
}