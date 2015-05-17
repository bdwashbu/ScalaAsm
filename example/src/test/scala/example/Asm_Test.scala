package example

import org.scalatest._

 import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.asm._
  import com.scalaAsm.x86.Operands._
  import com.scalaAsm.x86.Instructions._
class AsmTest extends FlatSpec with ShouldMatchers with Registers with Formats {
  
  "it" should "not compile aesm" in {

     "asm\"mov ebx 4\"" shouldNot compile
     "asm\"mov ebx, byte 4\"" shouldNot compile  
     "asm\"movebx, 4\"" shouldNot compile
     "asm\"mov ebx, 2147483648\"" shouldNot compile
     "asm\"add ebx, byte 256\"" shouldNot compile
     
     "asm\"mov ebx, 4\"" should compile
     "asm\"mov ebx,4\"" should compile
     "asm\"mov ebx,4  \"" should compile
     "asm\"  mov ebx,4  \"" should compile
     "asm\"  mov ebx, 4\"" should compile
     "asm\"mov ebx, 4  \"" should compile
     "asm\"mov ebx,   4\"" should compile
     "asm\"mov   ebx, 4\"" should compile
     "asm\"mov ebx, 2147483647\"" should compile
     "asm\"add ebx, byte 255\"" should compile
     
     "asm\"mov eax, [esp + 4]\"" should compile
     "asm\"mov eax, [esp+4]\"" should compile
     "asm\"mov eax, [  esp+4]\"" should compile
     "asm\"mov eax, [esp+4  ]\"" should compile
     "asm\"mov eax, [esp + 4]  \"" should compile
     "asm\"mov eax,   [esp + 4]\"" should compile

  }
}