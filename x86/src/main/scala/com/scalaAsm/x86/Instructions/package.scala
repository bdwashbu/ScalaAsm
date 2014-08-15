package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.Memory.Relative
import com.scalaAsm.x86.Operands.Constant
import com.scalaAsm.x86.Operands._

package object Instructions {

  trait NP

  trait M
  trait O
  trait I
  trait Offset

  trait MR
  trait OI
  trait RM
  trait M1
  trait MI
  
  type rel16 = Relative { type Size = WordOperand }
  type rel32 = Relative { type Size = DwordOperand }
}