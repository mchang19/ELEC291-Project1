$NOLIST
$MODLP51
$LIST

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

 
PB0        	equ P0.0
PB1			equ P0.1
PB2			equ P0.2
PB3			equ P0.3
PB4			equ P0.4
PB5			equ P0.5
PB6			equ P0.6
MODE_BUTTON    		equ P0.7

; Reset vector
org 0x0000
    ljmp main



; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	
;;;;;declaring variables such as set parameters, seconds, mode_sel etc
dseg at 30H
Count1ms:		ds 2
BCD_counter1:	ds 1
Reflow_time:	ds 1;
Reflow_temp:	ds 1;
Soak_time:		ds 1;
Soak_temp:		ds 1;
Mode_sel:     	ds 1;

ReflowTemp_UB:	ds 1;
ReflowTemp_LB:	ds 1;
ReflowTime_UB:	ds 1;
ReflowTime_LB:	ds 1;
SoakTemp_UB:	ds 1;
SoakTemp_LB:	ds 1;
SoakTime_UB:	ds 1;
SoakTime_LB:	ds 1;

;;;;;flags (?) using states so maybe uneeded
bseg
one_seconds_flag: dbit 1 ; Set to one in the ISR every time 1000 ms had passed
;PB0: dbit 1 ; Variable to store the state of pushbutton 0 after calling ADC_to_PB below TIME INC
;PB1: dbit 1 ; Variable to store the state of pushbutton 1 after calling ADC_to_PB below TIME DEC
;PB2: dbit 1 ; Variable to store the state of pushbutton 2 after calling ADC_to_PB below TEMP INC
;PB3: dbit 1 ; Variable to store the state of pushbutton 3 after calling ADC_to_PB below TEMP DEC
;PB4: dbit 1 ; Variable to store the state of pushbutton 4 after calling ADC_to_PB below MODE
;PB5: dbit 1 ; Variable to store the state of pushbutton 5 after calling ADC_to_PB below STOP
;PB6: dbit 1 ; Variable to store the state of pushbutton 6 after calling ADC_to_PB below START

; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5

cseg
$NOLIST
$include(LCD_4BIT.inc)
; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Welcome! To cont', 0
ToContinueClick:  db 'pls click mode  ', 0
TP:				  db 'Tp:', 0
Celsius:		  db 'C ', 0  
ReflowMessage:    db 'Reflow Settings:', 0
SoakMessage:      db 'Soak Settings:  ', 0
OvenDisplay:      db 't=   s tmp=   °C', 0
OvenDisplay2:     db 's:     otmp=  °C', 0
ConfirmStart:	  db 'Begin?	      ', 0
;rfl, sk, rps, rpp, coo




;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret
	
	
;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if a second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), cant_reach ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), cant_reach
	sjmp one_millisecond


cant_reach:
	ljmp Timer2_ISR_done
	
one_millisecond:
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb one_seconds_flag ; Let the main program know one second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	sjmp Main_Counter
	
Main_Counter:
	; Increment the BCD counter
;	mov a, BCD_counter1
;	cjne a, #0x59, Seconds_Inc 	; Checks if 60 seconds have passed
;	mov a, #0x00				; Once hit 60 seconds, reset to 00 seconds
;	mov BCD_counter1, a
	
Seconds_Inc:
	mov a, BCD_counter1
	add a, #0x01
	da a
	mov BCD_counter1, a
	clr a 

Dont_Inc:
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.


	
main:
	; Initialization
    mov SP, #0x7F
    lcall LCD_4BIT
    lcall Timer2_Init
    ;lcall ADC_to_PB
    
    ;Set mode for parameter set-up and intitialize all constants
    mov a, #0
    mov Mode_sel, a
    mov a, #0xFF
    mov ReflowTemp_UB, a
    mov a, #0xFF
    mov ReflowTemp_LB, a
    mov a, #0xFF
    mov ReflowTime_UB, a
    mov a, #0xFF
    mov ReflowTime_LB, a
    mov a, #0xFF
    mov SoakTemp_UB, a
    mov a, #0xFF
    mov SoakTemp_LB, a
    mov a, #0x90
    mov SoakTime_UB, a
    mov a, #0x60
    mov SoakTime_LB, a
    

    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
   
    ;set reflow and soak parameters
    ljmp defaultMessageDisplay
    

	Set_Cursor(1,1)
    Send_Constant_String(#OvenDisplay)
    Set_Cursor(1, 10)
    Send_Constant_String(#OvenDisplay2)
    
    setb one_seconds_flag
    
	
;------------------------------------------------------------    
defaultMessageDisplay:
    ;WriteCommand(#0x01)
    ;Wait_Milli_Seconds(#2)

    Set_Cursor(1, 1)
	Send_Constant_String(#Initial_Message)
    Set_Cursor(2, 1)
    Send_Constant_String(#ToContinueClick)
    

checkContinue:
	Set_Cursor(1, 1)
    jb PB5, checkContinue  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB5, checkContinue   ; if the 'BOOT' button is not pressed repeat
	jnb PB5, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ;ljmp which_Mode

;selectLanguage: To Be added later

setSoak:
;	WriteCommand(#0x01)
;   Wait_Milli_Seconds(#2)

    Set_Cursor(1, 1)
	Send_Constant_String(#SoakMessage)

    Set_Cursor(2,1)
    WriteData(#Soak_temp)
    WriteData(#0b11011111) ; degree sign 
    Send_Constant_String(#Celsius)

    Set_Cursor(2,14)
    WriteData(#Soak_time)
    WriteData(#'s')

checkSoakTimeINC:
    jb PB0, checkSoakTimeDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB0, checkSoakTimeDEC   ; if the button is not pressed jump
	jnb PB0, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Soak_time
    cjne a, #120, jumpINCSoakTime

checkSoakTimeDEC:
    jb PB1, checkSoakTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB1, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb PB1, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Soak_time
    cjne a, #60, jumpDECSoakTime


setSoakJump:		;can't reach branch
	ljmp setSoak
	
checkSoakTempINC:
    jb PB2, checkSoakTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB2, checkSoakTempDEC   ; if the 'BOOT' button is not pressed repeat
	jnb PB2, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Soak_temp
    cjne a, #200, jumpINCSoakTemp

checkSoakTempDEC:
    jb PB3, continueSoakSetting  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB3, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb PB3, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Soak_temp
    cjne a, #140, jumpDECSoakTemp

continueSoakSetting:
    jb PB4, setSoakJump  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB4, setSoakJump   ; if the 'BOOT' button is not pressed repeat
	jnb PB4, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ljmp setReflow
;----------------------------------------------
which_Mode:
	mov a, Mode_sel
	cjne a, #0x01, which_Mode2
	ljmp setSoak
	
which_Mode2:
	mov a, Mode_sel
	cjne a, #0x02, which_Mode3
	ljmp setReflow

which_Mode3:
	mov a, Mode_sel
	cjne a, #0x03, which_Mode
	ljmp activateOven

;-----------------------------------------------
	
jumpINCSoakTime:
    ljmp INCSoakTime

jumpDECSoakTime:
    ljmp DECSoakTime

jumpINCSoakTemp:
    ljmp INCSoakTemp

jumpDECSoakTemp:
    ljmp DECSoakTemp
;----------------------------------------------------------------------------------------------------------
setReflow:
	WriteCommand(#0x01)
    Wait_Milli_Seconds(#2)
    Set_Cursor(1, 1)
	Send_Constant_String(#ReflowMessage)

    Set_Cursor(2,1)
    Send_Constant_String(#TP)
    WriteData(#Reflow_temp)
    WriteData(#0b11011111)
    Send_Constant_String(#Celsius)
    WriteData(#Reflow_time)
    WriteData(#'s')

checkReflowTimeINC:
    jb PB0, checkReflowTimeDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB0, checkReflowTimeDEC   ; if the button is not pressed jump
	jnb PB0, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Reflow_time
    cjne a, #0x60, jumpINCReflowTime

checkReflowTimeDEC:
    jb PB1, checkReflowTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB1, checkReflowTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb PB1, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Reflow_time
    cjne a, #0x30, jumpDECReflowTime

setReflowJump:
	ljmp setReflow
checkReflowTempINC:
    jb PB2, checkReflowTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB2, checkReflowTempDEC   ; if the 'BOOT' button is not pressed repeat
	jnb PB2, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Reflow_temp
    cjne a, #240, jumpINCReflowTemp

checkReflowTempDEC:
    jb PB3, checkReflowTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB3, checkReflowTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb PB3, $		; Wait for button release.  The '$' means: jump to same instruction.
    mov a, Reflow_temp
    cjne a, #230, jumpDECReflowTemp

continueReflowSetting:
    jb PB4, setReflowJump  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB4, setReflowJump   ; if the 'BOOT' button is not pressed repeat
	jnb PB4, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ljmp activateOven
;----------------------------------------------
jumpINCReflowTime:
    ljmp INCReflowTime

jumpDECReflowTime:
    ljmp DECReflowTime

jumpINCReflowTemp:
    ljmp INCReflowTemp

jumpDECReflowTemp:
    ljmp DECReflowTemp
;----------------------------------------------------------------------------------------------------------
activateOven:
	WriteCommand(#0x01)
    Wait_Milli_Seconds(#2)

	Set_Cursor(1, 1)
	Send_Constant_String(#ConfirmStart)

	jb PB6, activateOven  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PB6, activateOven   ; if the 'BOOT' button is not pressed repeat
	jnb PB6, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.
	
	ret
;----------------------------------------------------------------------------------------------------------

INCSoakTime:
    mov a, Soak_time
    add a, #0x05
    mov Soak_time, a
    ljmp setSoak

DECSoakTime:
    mov a, Soak_time
    add a, #-0x05
    mov Soak_time, a
    ljmp setSoak

INCSoakTemp:
    mov a, Soak_temp
    add a, #0x05
    mov Soak_temp, a
    ljmp setSoak

DECSoakTemp:
    mov a, Soak_temp
    add a, #-0x05
    mov Soak_temp, a
    ljmp setSoak

;-------------------------
INCReflowTime:
    mov a, Reflow_time
    add a, #0x05
    mov Reflow_time, a
    ljmp setReflow

DECReflowTime:
    mov a, Reflow_time
    add a, #-0x05
    mov Reflow_time, a
    ljmp setReflow

INCReflowTemp:
    mov a, Reflow_temp
    add a, #0x05
    mov Reflow_temp, a
    ljmp setReflow

DECReflowTemp:
    mov a, Reflow_temp
    add a, #-0x05
    mov Reflow_temp, a
    ljmp setReflow
    	
END
