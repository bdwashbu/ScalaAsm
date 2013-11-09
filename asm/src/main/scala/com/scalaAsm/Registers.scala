package com.scalaAsm.asm

import com.scalaAsm.x86.x86Registers._

trait Registers {
    val edi = new EDI()
    val eax = new EAX()
    val ecx = new ECX()
    val ebp = new EBP()
    val edx = new EDX()
    val esp = new ESP()
    
    val cl = new CL()
    
    val es = new ES()
    val cs = new CS()
    val ss = new SS()
    val ds = new DS()
    
    val ax = new AX()
    val cx = new CX()
    val dx = new DX()
    
    val ah = new AH()
}