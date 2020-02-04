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
Show_Date:		  db '22/1/2020  Thurs', 0
Alarm_Clock:	  db 'Alarm Clock  ',0
Setting_Alarm:    db 'Setting Alarm',0


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

    mov a, mode ;increment mode
    add a, #0x01
    mov mode, a

;selectLanguage: To Be added later


