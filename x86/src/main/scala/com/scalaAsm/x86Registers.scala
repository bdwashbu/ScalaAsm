package com.scalaAsm.x86

object x86Registers
{
	abstract class Register[T](val ID: Byte)
    case class RegisterOffset[+T <: Register[_], S <: Immediate[_,_]](val offset: S, val x: T)
    
    trait selfRef[X <: Register[_]] {
      self: X =>
      def -[T <: Immediate[_,T]](offset: T) = RegisterOffset[X, T](offset.negate,this)
	  def +[T <: Immediate[_,T]](offset: T) = RegisterOffset[X, T](offset,this)
    }
	
	abstract class r8(reg: Byte) extends Register[Byte](reg)
	abstract class r16(reg: Byte) extends Register[Short](reg)
	abstract class r32(reg: Byte) extends Register[Int](reg)
	abstract class r64(reg: Byte) extends Register[Long](reg)
  
    case class AL extends r8(0) with selfRef[AL]
    case class CL extends r8(1) with selfRef[CL]
    case class DL extends r8(2) with selfRef[DL]
    case class BL extends r8(3) with selfRef[BL]
    case class AH extends r8(4) with selfRef[AH]
    case class CH extends r8(5) with selfRef[CH]
    case class DH extends r8(6) with selfRef[DH]
    case class BH extends r8(7) with selfRef[BH]
	
	// Segment registers
	case class ES extends r8(0) with selfRef[ES]
    case class CS extends r8(1) with selfRef[CS]
	case class SS extends r8(2) with selfRef[SS]
    case class DS extends r8(3) with selfRef[DS]
	case class FS extends r8(3) with selfRef[FS]
	case class GS extends r8(3) with selfRef[GS]

    case class AX extends r16(0) with selfRef[AX]
    case class CX extends r16(1) with selfRef[CX]
    case class DX extends r16(2) with selfRef[DX]
    case class BX extends r16(3) with selfRef[BX]
    case class SP extends r16(4) with selfRef[SP]
    case class BP extends r16(5) with selfRef[BP]
    case class SI extends r16(6) with selfRef[SI]
    case class DI extends r16(7) with selfRef[DI]
	
    // Accumulator for operands and results
	case class EAX extends r32(0) with selfRef[EAX]

	// Counter for string and loop operations
	case class ECX extends r32(1) with selfRef[ECX]

	// I/O pointer
	case class EDX extends r32(2) with selfRef[EDX]
	
	// Pointer to data in the DS segment
	case class EBX extends r32(3) with selfRef[EBX]
	
	// Stack pointer
	case class ESP extends r32(4) with selfRef[ESP]
	
	// Pointer to data on the stack
	case class EBP extends r32(5) with selfRef[EBP]

	// Pointer to data in the segment pointed to by the DS register
	case class ESI extends r32(6) with selfRef[ESI]
	
	// Pointer to data (or destination) in the segment pointed to by ES register
	case class EDI extends r32(7) with selfRef[EDI]
}