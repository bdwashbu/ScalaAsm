package com.scalaAsm.x86
package Operands

import com.scalaAsm.utils.Endian

protected[x86] trait AddressingFormSpecifier {
  type Displacement32 = EBP
  type AddressInSib = ESP
  type NoneSib = ESP
  
  val modRM: Option[ModRM]
  val sib: Option[SIB]
  val displacment: Option[Displacement]
  val immediate: Option[Immediate]

  lazy val getBytes: Array[Byte] = {
    val components: List[Option[InstructionField]] = List(modRM, sib, displacment, immediate)
    components.flatten.flatMap(_.getBytes).toArray
  }

  lazy val size: Int = {
//    val components = List(sib, displacment, immediate)
//    (modRM match {
//      case (Some(modRM)) => 1
//      case _ => 0
//    }) + components.flatten.map(imm => imm.size).sum
    getBytes.size
  }
}

object ModRM {
  type rm = RegisterOrMemory
  type reg = GPR
}

sealed class RegisterMode(val value: Byte)
case object NoDisplacement extends RegisterMode(0) // If r/m is 110, Displacement (16 bits) is address; otherwise, no displacemen
case object DisplacementByte extends RegisterMode(1) // Eight-bit displacement, sign-extended to 16 bits
case object DisplacementDword extends RegisterMode(2) // 32-bit displacement (example: MOV [BX + SI]+ displacement,al)
case object OtherRegister extends RegisterMode(3) // r/m is treated as a second "reg" field

case class ModRM(mod: RegisterMode, rm: GPR, reg: Option[GPR] = None, opEx: Option[Byte] = None) extends InstructionField {
  def getBytes = {
    (reg, opEx) match {
      case (_, Some(opEx)) => Array(((mod.value << 6) + (opEx << 3) + rm.ID).toByte)
      case (_, _) => Array(((mod.value << 6) + (reg.get.ID << 3) + rm.ID).toByte)
    }
  }
}

sealed class SIBScale(val value: Byte)
case object One extends SIBScale(0) // If r/m is 110, Displacement (16 bits) is address; otherwise, no displacemen
case object Two extends SIBScale(1) // Eight-bit displacement, sign-extended to 16 bits
case object Four extends SIBScale(2) // 32-bit displacement (example: MOV [BX + SI]+ displacement,al)
case object Eight extends SIBScale(3) // r/m is treated as a second "reg" field

case class SIB(scale: SIBScale, index: GPR, base: GPR) extends InstructionField {
  def getBytes = Array(((scale.value << 6) + (index.ID << 3) + base.ID).toByte)
}
