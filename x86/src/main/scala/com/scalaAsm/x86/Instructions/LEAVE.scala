package com.scalaAsm.x86.Instructions

import com.scalaAsm.x86.{Instruction, OneOpcode}

trait LEAVE

object LEAVE extends Instruction {
	val operands = NA
	val opcode = OneOpcode(0xC9)
}