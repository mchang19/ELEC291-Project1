$NOLIST
$MODLP51
$include(LCD_4bit.inc)
$include(math32.inc)
$LIST
org 0000H
   ljmp MainProgram

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
error_flag:        dbit 1 ; Set to one in the ISR every time 1000 ms had passed 
speak_flag:        dbit 1 ;
activation_flag:   dbit 1 ;
soaking_flag:      dbit 1 ;
rampUp_flag:       dbit 1 ;
reflow_flag:       dbit 1 ;
coolDown_flag:     dbit 1 ;
finished_flag:     dbit 1 ;

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Welcome! To cont', 0
ToContinueClick:  db 'pls click mode  ', 0

SoakMessage:      db 'Soak Settings:  ', 0
Setting_Alarm:    db 'Setting Alarm',0


+Outside_temp: 	ds 1;
Oven_temp:	ds 1;
Reflow_time:	ds 1;
Reflow_temp:	ds 1;
Soak_time:	ds 1;
Soak_temp:	ds 1;
Mode_sel:     	ds 1 ;

defaultMessageDisplay:
    lcall LCD_4BIT
    Set_Cursor(1, 0)
	Send_Constant_String(#InitMessage)
    Set_Cursor(2, 0)
    Send_Constant_String(#ToContinueClick)

checkContinue:
    jb THE MODE BUTTON, checkContinue  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb THE MODE BUTTON, checkContinue   ; if the 'BOOT' button is not pressed repeat
	jnb THE MODE BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a

;selectLanguage: To Be added later

setSoak:
    Set_Cursor(1, 0)
	Send_Constant_String(#SoakMessage)

    Set_Cursor(2,0)
    WriteData(#Soak_temp)
    WriteData(#0b11011111)
    WriteData(#'C   ')
    WriteData(#Soak_time)
    WriteData(#'s')

checkSoakTimeINC:
    jb INCREMENT_BUT, checkSoakTimeDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkSoakTimeDEC   ; if the button is not pressed jump
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.

    mov a, Soak_time
    add a, #0x05
    mov Soak_time, a
    ljmp checkSoakTempINC

checkSoakTimeDEC:
    jb INCREMENT_BUT, checkSoakTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    
    cjne Soak_time, #0x140, DECSoakTime

DECSoakTime:
    mov a, Soak_time
    sub a, #0x05
    mov Soak_time, a


