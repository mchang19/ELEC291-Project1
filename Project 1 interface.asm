; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

;----------------;
;button variables;
;----------------;

START_BUTTON   		equ P4.5
STOP_BUTTON       	equ P0.0
BUTTON1      		equ P0.1
BUTTON2       		equ P0.2
BUTTON3       		equ P0.3
BUTTON4       		equ P0.4
BUTTON5       		equ P0.5
BUTTON6       		equ P0.6
BUTTON7       		equ P0.6
BUTTON8       		equ P0.6
BUTTON9       		equ P0.6
BUTTON10      		equ P0.6
BUTTON11      		equ P0.6
BUTTON12      		equ P0.6
BUTTON13      		equ P0.6
RESET_BUTTON    	equ P0.6

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

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

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     	ds 2 ; Used to determine when half second has passed
BCD_counter: 	ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
Outside_temp: 	ds 1;
Oven_temp:		ds 1;
Reflow_time:	ds 1;
Reflow_temp:	ds 1;
Soak_time:		ds 1;
Soak_temp:		ds 1;
Mode_sel:     	ds 2;
ReflowTemp_UB:	ds 8;
ReflowTemp_LB:	ds 8;
ReflowTime_UB:	ds 7;
ReflowTime_LB:	ds 7;
SoakTemp_UB:	ds 8;
SoakTemp_LB:	ds 8;
SoakTime_UB:	ds 7;
SoakTime_LB:	ds 7;

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


cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     	    1234567890123456    <- This helps determine the location of the counter
Select_Language: 	db "Select language:", 0
Error			db "ERROR", 0
Time: 			db "Time:", 0
Temp:			db "Temp:", 0
TJ:				db "TJ:", 0
TO:				db "TO:", 0
Setting:		db "Setting", 0
Activation: 	db "ACTIVATION", 0
Ramp_Up:		db "RAMP UP", 0
Soaking: 		db "SOAKING", 0
Reflow:			db "REFLOW"
Cool_Down:		db "COOL DOWN", 0

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'Welcome! To cont', 0
ToContinueClick:  db 'pls click mode  ', 0

SoakMessage:      db 'Soak Settings:  ', 0
OvenDisplay:      db 't=   s tmp=   °C', 0
OvenDisplay2:     db 's:     otmp=  °C', 0

;rfl, sk, rps, rpp, coo

 
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    	clr ET0  ; Enable timer 0 interrupt
    	setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti

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
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), offset10 ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), offset10
	sjmp millisecond_cnt

offset10:
	ljmp Timer2_ISR_done

millisecond_cnt:
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know 1 second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
check_Alarm:
	mov a, Day
	cjne a, #0x07, Weekday_Alarm
	cjne a, #0x06, Weekday_Alarm
	mov a, Hour
	cjne a, Hour3, Main_counter
	mov a, Minute
	cjne a, Minute3, Main_counter
	mov a, AM_flag
	cjne a, AM_flag3, Main_counter
	ljmp Alarm_on
	
Weekday_Alarm:
	mov a, Hour
	cjne a, Hour2, Main_counter
	mov a, Minute
	cjne a, Minute2, Main_counter
	mov a, AM_flag
	cjne a, AM_flag2, Main_counter
	ljmp Alarm_on
	
Alarm_on:
	clr a
	setb ET0 
	
Main_counter:	
	; Increment the BCD counter
	mov a, BCD_counter
	cjne a, #0x59, Increment_Sec ; Checks if 60 seconds has passed 
	mov a, #0x00
	mov BCD_counter, a
	
	clr a
	mov a, Minute 
	add a, #0x01 ; Increments 1 minute if 60 seconds passed
	da a
	mov Minute, a  
	
	clr a
	mov a, Minute
    cjne a, #0x60, Not_increment ; Checks if 60 minutes has passed
	mov a, #0x00
	mov Minute, a
	
	clr a
	mov a, Hour 
	add a, #0x01 ; Increments 1 hour if 60 seconds passed
	da a
	mov Hour, a

	clr a
	mov a, Hour 
	cjne a, #0x13, Hour_increment ; Checks if 12 hours has passed
	mov a, #0x01 ;----
	mov Hour, a ;-----
	
Hour_increment:
	clr a
	mov a, Hour 
	cjne a, #0x12, Not_increment ; Checks if 12 hours has passed
	mov a, #0x01 ;----
	cpl AM_Flag ;'nots' the AM_Flag if 12
	jb AM_Flag, Day_increment
	sjmp Not_increment
	
Day_increment:	
	clr a
	mov a, Day
	add a, #0x01 ; Increments 1 full-day if AM->PM
	da a
	mov Day, a
	
	clr a
	mov a, Day
	cjne a, #0x08, Not_increment ; Checks if 7 days have passed 
	mov a, #0x01
	mov Day, a
	
	clr a	
	
Increment_Sec:
	mov a, BCD_counter
	add a, #0x01
	da a
	mov BCD_counter, a
	clr a
	
Not_increment:
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.




; Configure the serial port and baud rate
InitSerialPort:
    ; Since the reset button bounces, we need to wait a bit before
    ; sending messages, otherwise we risk displaying gibberish!
    mov R1, #222
    mov R0, #166
    djnz R0, $   ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, $-4 ; 22.51519us*222=4.998ms
    ; Now we can proceed with the configuration
	orl	PCON,#0x80
	mov	SCON,#0x52
	mov	BDRCON,#0x00
	mov	BRL,#BRG_VAL
	mov	BDRCON,#0x1E ; BDRCON=BRR|TBCK|RBCK|SPD;
    ret
    
INIT_SPI:     
	setb MY_MISO    ; Make MISO an input pin     
	clr MY_SCLK     ; For mode (0,0) SCLK is zero     
	ret   
DO_SPI_G:     
	push acc     
	mov R1, #0      ; Received byte stored in R1     
	mov R2, #8      ; Loop counter (8-bits) 
DO_SPI_G_LOOP:     
	mov a, R0       ; Byte to write is in R0     
	rlc a           ; Carry flag has bit to write     
	mov R0, a     
	mov MY_MOSI, c     
	setb MY_SCLK    ; Transmit     
	mov c, MY_MISO  ; Read received bit     
	mov a, R1       ; Save received bit in R1     
	rlc a     
	mov R1, a     
	clr MY_SCLK     
	djnz R2, DO_SPI_G_LOOP     
	pop acc     
	ret 
	
Delay_one_second:
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	ret

; Send a character using the serial port
putchar:
    jnb TI, putchar
    clr TI
    mov SBUF, a
    ret

; Send a constant-zero-terminated string using the serial port
SendString:
    clr A
    movc A, @A+DPTR
    jz SendStringDone
    lcall putchar
    inc DPTR
    sjmp SendString
SendStringDone:
    ret



;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    
    Set_Cursor(1,1)
    Send_Constant_String(#Temp)
    Set_Cursor(1, 10)
    Send_Constant_String(#TJ)
    Set_Cursor(2, 1)
    Send_Constant_String(#Time)
    
    mov a, #0
    mov Mode_sel, a
    
    mov a, #240
    mov ReflowTemp_UB, a
    
    mov a, #230
    mov ReflowTemp_LB, a
    
    mov a, #60
    mov ReflowTime_UB
    
    mov a, #30
    mov ReflowTime_LB
    
    mov a, #200
    mov SoakTemp_UB
    
    mov a, #140
    mov SoakTemp_LB
    
    mov a, #90
    mov SoakTime_UB
    
    mov a, #60
    mov SoakTime_LB
    
    clr error_flag
    clr speak_flag
    clr activation_flag
    clr soaking_flag
    clr rampUp_flag
    clr reflow_flag
    clr coolDown_flag
    clr finished_flag
    
    lcall InitSerialPort
	lcall defaultMessageDisplay
   	lcall INIT_SPI

    sjmp Forever
	

Forever:
	clr CE_ADC
	
	mov R0, #00000001B
	lcall DO_SPI_G
	
	mov R0, #10000000B
	lcall DO_SPI_G
	
	mov a, R1
	anl a, #00000011B
	mov Result+1, a
	
	mov R0, #55H
	lcall DO_SPI_G
	
	mov Result, R1
	
	setb CE_ADC
	lcall Delay
	lcall Calculate_Temp
	sjmp Forever


Calculate_Temp:

	mov x, Result
	mov x2, Result
	mov x3, Result
	
	mov x+1, Result+1
	mov x2+1, Result+1
	mov x3+1, Result+1
	
	mov x+2, #0

	
	mov x+3, #0


;ADC to Celsius
	Load_y(410)
	lcall mul32
	Load_y(1023)
	lcall div32
	Load_y(273)
	lcall sub32
	
;Do same thing to both temp readings (Hot and cold)
	mov x, ;something
	Load_y(___)
	lcall add32
	
	
	;change number to BCD for display
;for oven temp
	lcall hex2bcd
	Send_BCD(bcd)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar

	Set_Cursor(1,6)
	Display_BCD(bcd)
	Set_Cursor(1,9)
	Display_char(#'C')


;for outside temp
	lcall hex2bcd
	Send_BCD(bcd)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar

	Set_Cursor(1,6)
	Display_BCD(bcd)
	Set_Cursor(1,9)
	Display_char(#'C')
	
	lcall SendString
	ret
--------------------------------------------------------------------------------------------------------------
defaultMessageDisplay:
    lcall LCD_4BIT
    WriteCommand(#0x01)
    Wait_Milli_Seconds(#2)
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
    cjne Soak_time, #0x120, jumpINCSoakTime

checkSoakTimeDEC:
    jb INCREMENT_BUT, checkSoakTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_time, #0x60, jumpDECSoakTime

checkSoakTempINC:
    jb INCREMENT_BUT2, checkSoakTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkSoakTempDEC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_temp, #0x200, jumpINCSoakTemp

checkSoakTempDEC:
    jb INCREMENT_BUT2, checkSoakTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkSoakTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Soak_temp, #0x140, jumpDECSoakTemp

continueSoakSetting:
    jb THE MODE BUTTON, setSoak  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb THE MODE BUTTON, setSoak   ; if the 'BOOT' button is not pressed repeat
	jnb THE MODE BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ljmp setReflow
----------------------------------------------
jumpINCSoakTime:
    ljmp INCSoakTime

jumpDECSoakTime:
    ljmp DECSoakTime

jumpINCSoakTemp:
    ljmp INCSoakTemp

jumpDECSoakTemp:
    ljmp DECSoakTemp
----------------------------------------------------------------------------------------------------------
setSoak:
    Set_Cursor(1, 0)
	Send_Constant_String(#ReflowMessage)

    Set_Cursor(2,0)
    WriteData(#'Tp:')
    WriteData(#Reflow_temp)
    WriteData(#0b11011111)
    WriteData(#'C ')
    WriteData(#Reflow_time)
    WriteData(#'s')

checkReflowTimeINC:
    jb INCREMENT_BUT, checkReflowTimeDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkReflowTimeDEC   ; if the button is not pressed jump
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_time, #0x60, jumpINCReflowTime

checkReflowTimeDEC:
    jb INCREMENT_BUT, checkReflowTempINC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT, checkReflowTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_time, #0x30, jumpDECReflowTime

checkReflowTempINC:
    jb INCREMENT_BUT2, checkReflowTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkReflowTempDEC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_temp, #0x260, jumpINCReflowTemp

checkReflowTempDEC:
    jb INCREMENT_BUT2, checkReflowTempDEC  ; if the button is not pressed jump
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb INCREMENT_BUT2, checkReflowTempINC   ; if the 'BOOT' button is not pressed repeat
	jnb INCREMENT_BUT2, $		; Wait for button release.  The '$' means: jump to same instruction.
    cjne Reflow_temp, #0x230, jumpDECReflowTemp

continueReflowSetting:
    jb THE MODE BUTTON, setReflow  ; if the 'MODE' button is not pressed repeat
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb THE MODE BUTTON, setReflow   ; if the 'BOOT' button is not pressed repeat
	jnb THE MODE BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'MODE' button has been detected.

    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ljmp activateOven
----------------------------------------------
jumpINCReflowTime:
    ljmp INCReflowTime

jumpDECReflowTime:
    ljmp DECReflowTime

jumpINCReflowTemp:
    ljmp INCReflowTemp

jumpDECReflowTemp:
    ljmp DECReflowTemp
----------------------------------------------------------------------------------------------------------

	
	 	
    END