START /WAIT CMD /C CALL "publish-x86.bat" 
START /WAIT CMD /C CALL "build-inst.bat"
START CMD /C CALL "update-asm.bat"


