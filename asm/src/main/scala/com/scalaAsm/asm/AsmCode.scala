package com.scalaAsm.asm

import scala.collection.mutable.ListBuffer
import com.scalaAsm.x86.Instructions._
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.x86.Operands._
import com.scalaAsm.x86.One
import com.scalaAsm.x86.MachineCode

trait CodeSegment extends Registers with AsmSegment[CodeToken] with Catalog {

    def imm8(x: Byte) = Immediate8(x)
    def imm16(x: Short) = Immediate16(x)
    def imm32(x: Int) = Immediate32(x)
    
    def byte(x: Byte) = Displacement8(x)
    def word(x: Short) = Displacement16(x)
    def dword(x: Int) = Displacement32(x)

    implicit def toToken(code: MachineCode) = InstructionToken(code)
    
    var parserPos: Int = 0

    implicit def toByte(x: Int) = x.toByte
    val One = new One{}
    

    def getRawBytes: Array[Byte] = {
       def getInstTokens(tokens: List[CodeToken]): List[InstructionToken] = tokens.flatMap { x => x match {
        case Procedure(name, code) => getInstTokens(code)
        case token: InstructionToken => List(token)
      }}
       
       getInstTokens(builder.toList).map{x => x.inst.code}.reduce(_ ++ _)
    }
    
    private def procRef(procName: String) = ProcRef(procName)

    def label(name: String) = Label(name)

    def align(to: Int, filler: Byte = 0xCC.toByte) = Align(to, filler, (parserPos) => (to - (parserPos % to)) % to)

    def push(param: String) = Reference(param)

    def jnz(labelRef: String)(implicit ev: JNZ_1[Immediate8]) = LabelRef(labelRef, ev) 

    def jz(labelRef: String)(implicit ev: JZ_1[Immediate8]) = LabelRef(labelRef, ev)

    def call(refName: String) = Reference(refName)

    def jmp(ref: String) = JmpRef(ref)

    def repeat(numTimes: Int, code: List[CodeToken]): CodeGroup = {
      val expanded = ListBuffer[CodeToken]()
      for (i <- 0 until numTimes) {
        expanded ++= code
      }
      CodeGroup(expanded.toList)
    }
}