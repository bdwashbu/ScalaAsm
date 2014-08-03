package com.scalaAsm.x86

import com.scalaAsm.x86.Operands.GeneralPurpose
import com.scalaAsm.x86.Operands.Constant
package object Instructions {

  private[Instructions] trait NP

  private[Instructions] trait M
  private[Instructions] trait O
  private[Instructions] trait I
  private[Instructions] trait Offset

  private[Instructions] trait MR
  private[Instructions] trait OI
  private[Instructions] trait RM
  private[Instructions] trait M1
  private[Instructions] trait MI
  
  type BaseIndex[X <: GeneralPurpose,Y <: Constant[_]] = X#BaseIndex[Y]
}