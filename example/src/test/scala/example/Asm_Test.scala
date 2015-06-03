package example

import org.scalatest._

 import com.scalaAsm.x86.Instructions.General._
  import com.scalaAsm.asm._
  import com.scalaAsm.x86.Instructions._
  import com.scalaAsm.asm.Tokens._
  import com.scalaAsm.x86.Operands.Memory.Indirect
  
class AsmTest extends FlatSpec with ShouldMatchers with Registers {
  
  "it" should "not compile aesm" in {

     "asm\"mov ebx 4\"" shouldNot compile
     "asm\"mov ebx,\"" shouldNot compile
     "asm\"mov ebx, \"" shouldNot compile
     "asm\"mov ebx, byte 4\"" shouldNot compile  
     "asm\"movebx, 4\"" shouldNot compile
     "asm\"mov ebx, 2147483648\"" shouldNot compile
     "asm\"add ebx, byte 256\"" shouldNot compile
     "asm\"add ebx, byte 0x100\"" shouldNot compile
     "asm\"add ebx, byte 0xFFFFFFFF\"" shouldNot compile
     "asm\"mov[esp+4], eax\"" shouldNot compile
     "asm\"mov [+4], eax\"" shouldNot compile
     "asm\"mov [-4], eax\"" shouldNot compile
     "asm\"mov [esp + 4 + 2], eax\"" shouldNot compile
     "asm\"mov [esp+,4] eax\"" shouldNot compile
     "asm\"mov [,esp+4] eax\"" shouldNot compile
     "asm\"labelTes:t\"" shouldNot compile
     "asm\"label Test:\"" shouldNot compile
     "asm\"label,Test:\"" shouldNot compile
     "asm\"labelTes:t:\"" shouldNot compile
     "asm\":labelTest\"" shouldNot compile
     "asm\"End\"" shouldNot compile
     "asm\"\"\"mov ebx,\n4\"\"\"" shouldNot compile
     "asm\"fld b\"" shouldNot compile
     
     "asm\"mov ebx, 4  //comment test\"" should compile
     "asm\"mov ebx, 4\"" should compile
     "asm\"mov ebx, 0x4\"" should compile
     "asm\"mov ebx,4\"" should compile
     "asm\"mov ebx,4  \"" should compile
     "asm\"  mov ebx,4  \"" should compile
     "asm\"  mov ebx, 0x4\"" should compile
     "asm\"mov ebx, 4  \"" should compile
     "asm\"mov ebx  , 4  \"" should compile
     "asm\"mov ebx,   0x4\"" should compile
     "asm\"mov   ebx, 4\"" should compile
     "asm\"mov ebx, 2147483647\"" should compile
     "asm\"add ebx, 0xFFFFFFFF\"" should compile
     "asm\"add ebx, byte 255\"" should compile
     "asm\"add ebx,byte 0xFF\"" should compile
     "asm\"retn 0xFFF\"" should compile
     "asm\"retn 4\"" should compile
     "asm\"add ebx,dword 255\"" should compile
     "asm\"\"\"\nmov ebx, 0x4\nadd ebx, 0xFFFFFFFF\"\"\"" should compile
     
     "asm\"mov eax, [esp + 4]\"" should compile
     "asm\"mov eax, [esp-4]\"" should compile
     "asm\"mov eax, [esp-0x4]\"" should compile
     "asm\"mov eax,[  esp+4]\"" should compile
     "asm\"mov eax, [esp-4  ]\"" should compile
     "asm\"mov eax, [esp + 4]  \"" should compile
     "asm\"mov eax,   [esp - 4]  \"" should compile
     "asm\"mov eax,[esp+4]\"" should compile
     
     "asm\"mov [esp + 4], eax\"" should compile
     "asm\"mov [esp+4], eax\"" should compile
     "asm\"mov [esp-4] , eax\"" should compile

     "asm\"mov eax, [esp]\"" should compile
     "asm\"mov eax,[esp]\"" should compile
     //"asm\"mov eax,[ebp]\"" shouldNot compile //no ebp mode
     //"asm\"mov [eax], esp\"" should compile  // NO FORMAT YET
     //"asm\"mov [eax]  ,esp\"" should compile // NO FORMAT YET
     
     "asm\"labelTest:\"" should compile
  }
}