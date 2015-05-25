package example

import org.scalatest._
import com.scalaAsm.asm._
import com.scalaAsm.asm.AsmProgram
import com.scalaAsm.asm.Tokens._
import com.scalaAsm.asm.DataSection
import com.scalaAsm.x86._
import com.scalaAsm.x86.Instructions._
import scala.reflect.runtime.universe._

import com.scalaAsm.x86.Instructions._
import com.scalaAsm.x86.Instructions.General._
import com.scalaAsm.x86.Operands._

class ArithmeticTest extends FlatSpec with ShouldMatchers with Formats {

  val executableName = "test_ArithmeticTest.exe"
  
  def getExecutable[O1 <: InstructionDefinition, O2](inst: TwoOperands[O1], input1: Int, input2: O2)(implicit ev: O1#_2[EAX, O2], format: TwoOperandFormat[EAX, O2]): AsmProgram = {
    new AsmProgram {

      sections += new DataSection(
        Variable("test", "%d\n\u0000")) {}

      sections += new CodeSection {

        builder += Code(
          asm"mov eax, $input1",
          List(inst(eax, input2)),
          asm"""push eax
          push test
          call printf
          pop eax
          pop eax
          retn"""
        )

      }
    }
  }

  "A simple 2-operand arithmetic test" should "print the correct results" in {

    getProgramOutput(getExecutable(ADD, 1, dword(1)), false) should equal("2")
    getProgramOutput(getExecutable(ADD, 1, dword(-3)), false) should equal("-2")
    getProgramOutput(getExecutable(SUB, 100, dword(10)), false) should equal("90")
    getProgramOutput(getExecutable(SHL, 1, byte(1)), false) should equal("2")
    getProgramOutput(getExecutable(SHL, 1, byte(4)), false) should equal("16")
    getProgramOutput(getExecutable(SHR, 1, byte(1)), false) should equal("0")
    getProgramOutput(getExecutable(SHR, 2, byte(1)), false) should equal("1")
    getProgramOutput(getExecutable(ROR, 1, byte(1)), false) should equal("-2147483648")
    getProgramOutput(getExecutable(ROL, -2147483648, byte(1)), false) should equal("1")
    getProgramOutput(getExecutable(AND, 1, byte(1)), false) should equal("1")
    getProgramOutput(getExecutable(AND, 1, byte(0)), false) should equal("0")
    getProgramOutput(getExecutable(OR, 1, byte(0)), false) should equal("1")
    getProgramOutput(getExecutable(XOR, 1, byte(0)), false) should equal("1")
    getProgramOutput(getExecutable(XOR, 0, byte(0)), false) should equal("0")
    getProgramOutput(getExecutable(XOR, 1, byte(1)), false) should equal("0")
  }
}