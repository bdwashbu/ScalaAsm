package com.scalaAsm.x86.AddressingMethods



sealed abstract class AddressingMethod(val abbreviation: String, val hasRMByte: Boolean) {
  override def toString = abbreviation
}
object DirectAddress extends AddressingMethod("ptr", false)
object MemoryAddressedbyAX extends AddressingMethod("m", false)
object MemoryAddressedbyAXPlusAL extends AddressingMethod("m", false)
object MemoryAddressedbyDS extends AddressingMethod("m", false)
object RegFieldSelectsControlRegister extends AddressingMethod("CRn", true)
object RegFieldSelectsDebugRegister extends AddressingMethod("DRn", true)
object ModRMByteRegisterOrMemory extends AddressingMethod("rm", true)
object ModRMByteX87StackOrMemory extends AddressingMethod("STi/m", true)
object ModRMByteX87StackRegister extends AddressingMethod("STi", true)
object FlagsRegister extends AddressingMethod("-", false)
object RegFieldRegister extends AddressingMethod("r", true)
object RMFieldRegisterAlways extends AddressingMethod("r", true)
object ImmediateData extends AddressingMethod("imm", false)
object RelativeOffset extends AddressingMethod("rel", false)
object ModRMByteMemoryOnly extends AddressingMethod("m", true)
object RMFieldMMXRegister extends AddressingMethod("mm", true)
object NoModRMByteOrSIBWithOffset extends AddressingMethod("moffs", false)
object RegFieldMMXRegister extends AddressingMethod("mm", true)
object ModRMByteMMXRegOrMemory extends AddressingMethod("mm/m64", true)
object ModFieldRegister extends AddressingMethod("r", true)
object RegFieldSegmentRegister extends AddressingMethod("Sreg", true)
object StackOperand extends AddressingMethod("-", false)
object RegFieldTestRegister extends AddressingMethod("TRn", true)
object RMField128XMM extends AddressingMethod("xmm", true)
object RegField128XMM extends AddressingMethod("xmm", true)
object ModRMByte128XXMOrMemory extends AddressingMethod("xmm/m", true)
object MemoryAddressedbySI extends AddressingMethod("m", false)
object MemoryAddressedbyDI extends AddressingMethod("m", false)
object OpcodeSelectsRegister extends AddressingMethod("r", false)