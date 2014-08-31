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
ARG -11D               ;STD_OUTPUT_HANDLE
INVOKE GetStdHandle       ;get, in eax, handle to active screen buffer
MOV RBX, RAX

ARG 0,ADDR RCKEEP      ;RCKEEP receives output from API
ARG 14D, ADDR Hello   
ARG RAX                ;handle to active screen buffer
INVOKE WriteFile

ARG 0,ADDR RCKEEP      ;RCKEEP receives output from API
ARG 29D, ADDR Key
ARG RBX                ;handle to active screen buffer
INVOKE WriteFile

ARG - 10D
INVOKE GetStdHandle
ARG RAX
INVOKE FlushConsoleInputBuffer

INVOKE _getch
Xor RAX, RAX             ;return zero
RET