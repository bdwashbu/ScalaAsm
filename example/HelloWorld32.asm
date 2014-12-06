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
DATA SECTION
;
RCKEEP DD 0             ;temporary place to keep things
Hello DB "Hello World!",0Dh,0Ah
Key DB "Press any key to continue ..."
;
CODE SECTION
;
START:
PUSH -11                ;STD_OUTPUT_HANDLE
CALL GetStdHandle       ;get, in eax, handle to active screen buffer
PUSH 0,RCKEEP        ;KEEP receives output from API
PUSH 14,ADDR Hello    ;24=length of string
PUSH EAX                ;handle to active screen buffer
CALL WriteFile

PUSH -11                ;STD_OUTPUT_HANDLE
CALL GetStdHandle       ;get, in eax, handle to active screen buffer
PUSH 0,RCKEEP         ;KEEP receives output from API
PUSH 29,ADDR Key    ;24=length of string
PUSH EAX                ;handle to active screen buffer
CALL WriteFile

CALL _getch
XOR EAX,EAX             ;return eax=0 as preferred by Windows
RET