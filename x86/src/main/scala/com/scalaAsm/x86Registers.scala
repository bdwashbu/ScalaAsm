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
  
    class AL extends r8(0) with selfRef[AL]
    class CL extends r8(1) with selfRef[CL]
    class DL extends r8(2) with selfRef[DL]
    class BL extends r8(3) with selfRef[BL]
    class AH extends r8(4) with selfRef[AH]
    class CH extends r8(5) with selfRef[CH]
    class DH extends r8(6) with selfRef[DH]
    class BH extends r8(7) with selfRef[BH]
	
	// Segment registers
	class ES extends r8(0) with selfRef[ES]
    class CS extends r8(1) with selfRef[CS]
	class SS extends r8(2) with selfRef[SS]
    class DS extends r8(3) with selfRef[DS]
	class FS extends r8(3) with selfRef[FS]
	class GS extends r8(3) with selfRef[GS]

    class AX extends r16(0) with selfRef[AX]
    class CX extends r16(1) with selfRef[CX]
    class DX extends r16(2) with selfRef[DX]
    class BX extends r16(3) with selfRef[BX]
    class SP extends r16(4) with selfRef[SP]
    class BP extends r16(5) with selfRef[BP]
    class SI extends r16(6) with selfRef[SI]
    class DI extends r16(7) with selfRef[DI]
	
    // Accumulator for operands and results
	class EAX extends r32(0) with selfRef[EAX]

	// Counter for string and loop operations
	class ECX extends r32(1) with selfRef[ECX]

	// I/O pointer
	class EDX extends r32(2) with selfRef[EDX]
	
	// Pointer to data in the DS segment
	class EBX extends r32(3) with selfRef[EBX]
	
	// Stack pointer
	class ESP extends r32(4) with selfRef[ESP]
	
	// Pointer to data on the stack
	class EBP extends r32(5) with selfRef[EBP]

	// Pointer to data in the segment pointed to by the DS register
	class ESI extends r32(6) with selfRef[ESI]
	
	// Pointer to data (or destination) in the segment pointed to by ES register
	class EDI extends r32(7) with selfRef[EDI]
}