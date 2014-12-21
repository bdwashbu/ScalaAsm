package com.scalaAsm.x86

package object Operands {

  type GPR = GeneralPurpose[_]
  
  abstract class x86Size[T: Numeric] {
    def size: Int
    def negate(value: T) = implicitly[Numeric[T]].negate(value)
  }
  
  implicit object x86ByteSize extends x86Size[_8] {
    def size = 1
  }
  
  implicit object x86WordSize extends x86Size[_16] {
    def size = 2
  }
  
  implicit object x86DwordSize extends x86Size[_32] {
    def size = 4
  }
  
  implicit object x86QwordSize extends x86Size[_64] {
    def size = 8
  }

}