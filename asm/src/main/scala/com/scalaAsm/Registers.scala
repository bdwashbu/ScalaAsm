package com.scalaAsm.asm

import com.scalaAsm.x86.x86Registers._

trait Registers {
    val edi = EDI()
    val eax = EAX()
    val ecx = ECX()
    val ebp = EBP()
    val edx = EDX()
    val esp = ESP()
    
    val cl = new CL()
    
    val es = ES()
    val cs = CS()
    val ss = SS()
    val ds = DS()
    
    val ax = AX()
    val cx = CX()
    val dx = DX()
    
    val ah = AH()
}