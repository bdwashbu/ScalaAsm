package com.scalaAsm.x86

import com.scalaAsm.x86.Operands._

object OperandEncoding {

  abstract class MI[M <: ModRM.reg, I <: imm](op1: M, op2: I) extends Instruction2[M, I] { 
     val operand1 = op1
     val operand2 = op2
  }
  
  abstract class RM[R <: ModRM.reg, M <: ModRM.rm](op1: R, op2: M) extends Instruction2[R,M] {
	  val operand1 = op1
	  val operand2 = op2 
  }
  
  abstract class O[R <: ModRM.reg](op1: R) extends Instruction1[R] {
   val operand1 = op1
   val operand2 = None
  }
  
  abstract class I[I <: Immediate](op1: I) extends Instruction1[I] {
   val operand1 = op1
   val opcode2 = None
  }
  
  abstract class M[M <: ModRM.rm](op1: M) extends Instruction1[M] {
       val operand1 = op1
       val operand2 = None
  }
  
  abstract class M1[M <: ModRM.rm](op1: M) extends Instruction1[M] {
     val operand1 = op1 
     val operand2 = None
  }
  
}
