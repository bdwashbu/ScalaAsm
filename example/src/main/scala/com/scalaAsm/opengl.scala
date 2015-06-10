package com.scalaAsm

import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.asm.x86_32
import com.scalaAsm.asm._
import com.scalaAsm.assembler.Assembler
import com.scalaAsm.linker.Linker
import java.io._

object main {
  def main(args: Array[String]): Unit = {
    try {

      val outputStream = new DataOutputStream(new FileOutputStream("opengl.exe"));
      val assembler = new Assembler {}
      val linker = new Linker {}

      var beginTime = System.nanoTime()
      
      val ogl = assembler.assemble(opengl).addIcon("scala.ico")
      val exe = linker.link(ogl, 0x3000, true, "kernel32.dll", "glut32.dll", "opengl32.dll")

      outputStream.write(exe.get)
      println("done generating in " + (System.nanoTime() - beginTime) / 1000000 + " ms")
      outputStream.close
    }
  }
}

object opengl extends AsmProgram {
  
  def hex(n: Int): String = {
    // call toUpperCase() if that's required
    String.format("0x%8s", Integer.toHexString(n)).replace(' ', '0');
  }
  
  import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.x86.Instructions.System._
  
  sections += new DataSection (
    Variable("one", hex(java.lang.Float.floatToRawIntBits(1.0.toFloat))),
    Variable("neghalf", hex(java.lang.Float.floatToRawIntBits(-0.5.toFloat))),
    Variable("half", hex(java.lang.Float.floatToRawIntBits(0.5.toFloat)))
  ) {}

  sections += new CodeSection {

//    builder += Code(asm""" 
//      push    dword 16384
//      call    glClear              // glClear(GL_COLOR_BUFFER_BIT)
//      push    dword 9
//      call    glBegin              // glBegin(GL_POLYGON)
//      push    dword 0
//      push    dword 0
//      push    dword 1.0
//      call    glColor3f           // glColor3f(1, 0, 0)
//      push    dword 0
//      push    dword neghalf
//      push    dword neghalf
//      call    glVertex3f          // glVertex(-.5, -.5, 0)
//      push    dword 0
//      push    dword one
//      push    dword 0
//      call    glColor3f           // glColor3f(0, 1, 0)
//      push    dword 0
//      push    dword neghalf
//      push    dword half
//      call    glVertex3f          // glVertex(.5, -.5, 0)
//      push    dword [one]
//      push    dword 0
//      push    dword 0
//      call    glColor3f           // glColor3f(0, 0, 1)
//      push    dword 0
//      push    dword [half]
//      push    dword 0
//      call    glVertex3f          // glVertex(0, .5, 0)
//      call    glEnd                // glEnd()
//      call    glFlush              // glFlush()
//      retn
//    """
//    )
  }
}