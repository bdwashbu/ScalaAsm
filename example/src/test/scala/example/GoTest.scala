package example

import org.scalatest._
import java.io._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import com.scalaAsm.asm._
import scala.xml._
import com.scalaAsm.asm.Tokens._

// Must have GoLink on path

class GoTest extends FlatSpec with ShouldMatchers {

  "A .o produced by scala x86" should "be linked by GoLink and print 'Hello World'" in {
    val name = System.nanoTime
    val assembler = new Assembler {}
    val linker = new Linker {}

    var beginTime = System.nanoTime()
    val obj = assembler.assemble(HelloWorld)
    obj.write("goLinkResult.obj")

    val rt = Runtime.getRuntime();

    val runGolink = new File("goTest.bat")
    val goWriter = new java.io.PrintWriter(runGolink)
    goWriter.println("cmd /c GoLink /mix /console goLinkResult.obj msvcrt.dll")
    goWriter.println("exit")
    goWriter.close
    goWriter.flush()

    rt.exec("cmd /c start " + runGolink.getAbsolutePath)

    Thread.sleep(1000)

    val runResult = Runtime.getRuntime().exec("goLinkResult.exe");

    val in = new BufferedReader(
      new InputStreamReader(runResult.getInputStream()));

    val goOutput = in.readLine()

    runResult.waitFor()

    runGolink.delete
    new File("goLinkResult.exe").delete

    goOutput should equal("Hello World!")
  }

  //val child = Runtime.getRuntime().exec(executableName);
}

class GccTest extends FlatSpec with ShouldMatchers {

  "A .o produced by scala x86" should "be linked by GCC and print 'Hello World'" in {
    val name = System.nanoTime
    val assembler = new Assembler {}
    val linker = new Linker {}

    var beginTime = System.nanoTime()
    val obj = assembler.assemble(HelloWorld)
    obj.write("goLinkResult.obj")

    val rt = Runtime.getRuntime();

    val runGCC = new File("gccTest.bat")
    val gccWriter = new java.io.PrintWriter(runGCC)
    gccWriter.println("cmd /c gcc -mconsole -nostdlib goLinkResult.obj -o goLinkResultgcc.exe -l msvcrt")
    gccWriter.println("exit")
    gccWriter.close
    gccWriter.flush()

    rt.exec("cmd /c start " + runGCC.getAbsolutePath)

    Thread.sleep(1000)

    val gccRunResult = Runtime.getRuntime().exec("goLinkResultgcc.exe");

    val gccIn = new BufferedReader(
      new InputStreamReader(gccRunResult.getInputStream()));

    val gccoutput = gccIn.readLine()

    gccRunResult.waitFor()

    runGCC.delete
    new File("goLinkResultgcc.exe").delete

    gccoutput should equal("Hello World!")
  }
}