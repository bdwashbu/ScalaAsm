;EasyCodeName=Module1,1
;------------------------------------------------------------------
;
;      HelloWorld1 - copyright Jeremy Gordon 2002
;
;      SIMPLE "HELLO WORLD" WINDOWS CONSOLE PROGRAM - for GoAsm
;
;      Assemble using GoAsm HelloWorld1 (produces PE COFF file)
;      Then link as windows console program using GoLink as follows:-
;      GoLink /console helloworld1.obj kernel32.dll
;      (add -debug coff if you want to watch the program in the debugger)
;
;      Note that the GetStdHandle and WriteFile calls are to kernel32.dll
;------------------------------------------------------------------
;
Data Section
;
RCKEEP DD 0             ;temporary place to keep things
Hello DB "Hello World!",0Dh,0Ah
Key DB "Press any key to continue ..."
;
CODE SECTION
;
START:
PUSH -11D               ;STD_OUTPUT_HANDLE
CALL GetStdHandle       ;get, in eax, handle to active screen buffer
MOV EBX, EAX

PUSH 0,ADDR RCKEEP      ;RCKEEP receives output from API
PUSH 14D, ADDR Hello   
PUSH EAX                ;handle to active screen buffer
Call WriteFile

PUSH 0,ADDR RCKEEP      ;RCKEEP receives output from API
PUSH 29D, ADDR Key
PUSH EBX                ;handle to active screen buffer
Call WriteFile

Push - 10D
Call GetStdHandle
Push Eax
Call FlushConsoleInputBuffer

Call _getch
Xor Eax, Eax             ;return zero
RET