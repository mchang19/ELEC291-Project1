$NOLIST
$MODLP51
$LIST

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5



;---------------;
;  Constants    ;
;---------------;
CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))


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
Count1ms:     	ds 2; Used to determine when half second has passed
BCD_counter: 	ds 1; The BCD counter incrememted in the ISR and displayed in the main loop
cold_temp: 		ds 1;
hot_temp:		ds 1;

Reflow_time:	ds 2;
Reflow_temp:	ds 2;
Soak_time:		ds 2;
Soak_temp:		ds 2;
Mode_sel:     	ds 2;



pwm:			ds 7;

Result: 		ds 2;
x:				ds 4;
y:				ds 4;
mf:				ds 1;

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

;--------------;

CE_ADC    EQU  P2.0 
MY_MOSI   EQU  P2.1  
MY_MISO   EQU  P2.2 
MY_SCLK   EQU  P2.3 

LCD_RS equ P0.5
LCD_RW equ P0.6
LCD_E  equ P0.7
LCD_D4 equ P1.2
LCD_D5 equ P1.3
LCD_D6 equ P1.4
LCD_D7 equ P1.6

;----------------;
;button variables;
;----------------;
PB0        			equ P0.0 ;time inc
PB1					equ P0.1 ;time dec
PB2					equ P0.2 ;temp inc
PB3					equ P0.3 ;temp dec
PB4					equ P0.4 ;increment mode from continueReflowSetting
PB5					equ P0.5 ;increment mode from continueSoakSetting
PB6					equ P0.6 ;start button 
MODE_BUTTON    		equ P0.7 ;mode

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$include(macros.inc)
$LIST

;                     	    1234567890123456    <- This helps determine the location of the counter
Select_Language:		db 'Select language:', 0
Error:					db 'ERROR', 0
Time:					db 'Time:', 0

TP:						db 'Tp:', 0
Celsius:				db 'C ', 0  
ReflowMessage:			db 'Reflow Settings:', 0

ConfirmStart:			db 'Begin?	      ', 0
;rfl, sk, rps, rpp, coo

Activation: 		db 'ACTIVATION', 0
Ramp_Up:		db 'RAMP UP', 0
Soaking: 		db 'SOAKING', 0
Reflow:			db 'REFLOW'
Cool_Down:		db 'COOL DOWN', 0

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
	jnb error_flag, notOn
    reti
notOn:
	clr SOUND_OUT
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
	sjmp Seconds_Inc
	
Increment_Sec:
	mov a, BCD_counter
	add a, #0x01
	da a
	mov BCD_counter, a
	clr a
	
Dont_Inc:
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.


;---------------;
; SPI and init  ;
;---------------;

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
;-------------------------------------------
	
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

;-----------------;
; Voltage to Temp ;
;-----------------;
ConvertTemp:
	mov x,   Result
	mov x+1, Result+1
	mov x+2, #0
	mov x+3, #0	
	
	;convert cold junction voltage to temperature
	Load_y(410)
	lcall mul32
	Load_y(1023)
	lcall div32
	Load_y(273)
	lcall sub32

	ret

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall LCD_4BIT
    lcall Timer2_Init
    lcall InitSerialPort
   	lcall INIT_SPI
   	
   	clr error_flag
    clr speak_flag
    clr activation_flag
    clr soaking_flag
    clr rampUp_flag
    clr reflow_flag
    clr coolDown_flag
    clr finished_flag
    
    mov Soak_temp, #low(SoakTemp_LB)
    mov Soak_temp+1, #high(SoakTemp_UB)
    mov Soak_time, SoakTime_LB
    mov Reflow_temp, #low(ReflowTemp_LB)
    mov Reflow_temp+1, #high(ReflowTemp_LB)
    mov Reflow_time, ReflowTime_LB
    
    ; In case you decide to use the pins of P0, configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    
    lcall defaultMessageDisplay
    Wait_Milli_Seconds(#50)
    lcall setReflow
    Wait_Milli_Seconds(#50)
    lcall activateOven  ;technically our 'state 0' 
    
	

forever:
	Read_Temp_Channel(0)
	lcall ConvertTemp
	lcall hex2bcd
	
	Wait_Milli_Seconds(#250)
	WriteCommand(#0x80)
	Send_Constant_String(#P_STATE)
	WriteCommand(#0x89)
	WriteData(#' ')
	lcall Set_LEDS
	
	;depending on what value 'state' contains, jump to that state 
	mov a, state
	cjne a, #0, next1
	ret

	lcall displayDefaultMessage

next1:
	mov a, state 
	cjne a, #1, next2
	sjmp state1
next2:
	mov a, state
	cjne a, #2, next3
	ljmp state2
next3:
	mov a, state
	cjne a, #3, next4
	ljmp state3
next4:
	mov a, state
	cjne a, #4, next4
	ljmp state4
next5:
	mov a, state
	cjne a, #5, next1
	ljmp state5

	
;state0:
;	cjne a, #0, state1
;	mov pwm, #0
;	jb PB6, state0_done
;	jnb PB6, $ ; Wait for key release
;	mov state, #1
;
;state0_done:
;	lcall forever

state1:
	cjne a, #1, state2
	mov pwm, #100
	;setb whatever pin for SSR
	mov sec, #0
	mov a, Soak_temp
	clr c
	subb a, temp
	lcall forever
	cjne time, #60, check_temp   ;auto termination thing
	
backtoState1:	
	jnc state1_done
	mov state, #2

check_temp:
Read_Temp_Channel(#0)
	lcall ConvertTemp
	load_y(30)
	lcall x_lteq_y

highTemp:
	jbc mf, abort
	sjmp	backtoState1


abort:
	setb error_flag
	clr OVEN_ON_PIN
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	Wait_Milli_Seconds(#200)
	ljmp main
	
state1_done:
	lcall forever

state2:
	cjne a, #2, state3
	mov pwm, #20
	
	lcall twenty_percent
	lcall twenty_percent2
	
	mov a, Soak_time
	clr c
	subb a, sec
	jnc state2_done
	mov state, #3
	
twenty_percent:
	Read_Temp_Channel(#0)
	lcall ConvertTemp
	mov x, bcd
	mov y, Soak_Temp
	lcall x_gt_y
	cjne mf, #1, go_back
	sjmp turn_off

twenty_percent2:
	Read_Temp_Channel(#0)
	lcall ConvertTemp
	mov x, bcd
	mov y, Soak_Temp
	lcall x_lt_y
	cjne mf, #1, go_back
	sjmp turn_on
	
turn_on:
	;setb whatever pin for SSR
	ret
	
turn_off:
	;clear whatever pin for SSR
	ret 
	
go_back:
	ret
	


state2_done:
	lcall forever
	
state3:
	cjne a, #3, state4
	mov pwm, #100
	;setb whatever pin for SSR 
	mov sec, #0
	mov a, Reflow_temp
	clr c
	subb a, temp
	jnc state3_done
	mov state, #4

state3_done:
	lcall forever
	
state4:
	cjne a, #4, state5
	mov pwm, #20
	
	lcall twenty_percent3
	lcall twenty_percent4
	
	mov a, Reflow_time
	clr c
	subb a, sec
	jnc state4_done
	mov state, #5
	
twenty_percent3:
	Read_Temp_Channel(#0)
	lcall ConvertTemp
	mov x, bcd
	mov y, Reflow_Temp
	lcall x_gt_y
	cjne mf, #1, go_back
	sjmp turn_off

twenty_percent4:
	Read_Temp_Channel(#0)
	lcall ConvertTemp
	mov x, bcd
	mov y, Reflow_Temp
	lcall x_lt_y
	cjne mf, #1, go_back
	sjmp turn_on
		
state4_done:
	lcall forever
	
state5:
	cjne a, #5, state0
	mov pwm, #0
	;clr whatever pin
	mov a, Reflow_temp
	clr c
	subb a, temp
	jnc state5_done
	mov state, #0
	
state5_done:
	lcall forever
		

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
	
    pushbuttons(#7)		;check if mode button pushed
    jz checkContinue;repeat this loop if not pressed
    
    mov a, Mode_sel ;increment mode
    add a, #0x01
    mov Mode_sel, a
    ;ljmp which_Mode

;selectLanguage: To Be added later

setSoak:
    Set_Cursor(1, 1)
	Send_Constant_String(#SoakMessage)

    Set_Cursor(2,1)
    Display_BCD(Soak_temp+1)
    Display_BCD(Soak_temp)
 
    WriteData(#0b11011111) ; degree sign 
    Send_Constant_String(#Celsius)
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#' ')
 	Display_BCD(Soak_time)
    WriteData(#'s')

checkSoakTimeINC:
	pushbuttons(#0)
	jz checkSoakTimeDEC
	
    mov a, Soak_time
    cjne a, #0x50, jumpINCSoakTime
    
    mov a, #0x30
    mov Soak_time, a


checkSoakTimeDEC:
	pushbuttons(#1)
    jz checkSoakTempINC
    
    mov a, Soak_time
    cjne a, #0x60, jumpDECSoakTime

setSoakJump:		;can't reach branch
	ljmp setSoak
	
checkSoakTempINC:
    pushbuttons(#2)
    jz checkSoakTempDEC
    mov a, Soak_temp
    cjne a, #200, jumpINCSoakTemp

checkSoakTempDEC:
    pushbuttons(#3)
    jz checkSoakTempINC
    mov a, Soak_temp
    cjne a, #140, jumpDECSoakTemp

continueSoakSetting:
	; A valid press of the 'MODE' button has been detected.
	pushbuttons(#5)
	jz setSoakJump
    mov a, Mode_sel ;increment mode
    add a, #0x01
    da a
    mov Mode_sel, a
    ret
;--------------------------------
	
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
	Wait_Milli_Seconds(#50)
    Set_Cursor(1, 1)
	Send_Constant_String(#ReflowMessage)

    Set_Cursor(2,1)
    Send_Constant_String(#TP)
    Display_BCD(Reflow_temp+1)
    Display_BCD(Reflow_temp)
    WriteData(#0b11011111)
    Send_Constant_String(#Celsius)
	WriteData(#' ')
	WriteData(#' ')
	WriteData(#' ')
 	Display_BCD(Reflow_time)
    WriteData(#'s')

checkReflowTimeINC:
    pushbuttons(#0)
    jz checkReflowTimeDEC
    mov a, Reflow_time
    cjne a, #0x90, jumpINCReflowTime

checkReflowTimeDEC:
	pushbuttons(#1)
	jz checkReflowTempINC
    mov a, Reflow_time
    cjne a, #0x00, jumpDECReflowTime

setReflowJump:
	ljmp setReflow
checkReflowTempINC:
    pushbuttons(#2)
    jz checkReflowTempDEC
    mov a, Reflow_temp
    cjne a, #0x90, jumpINCReflowTemp

checkReflowTempDEC:
    pushbuttons(#3)
    jz checkReflowTempINC
    
    mov a, Reflow_temp
    cjne a, #0x00, jumpDECReflowTemp

continueReflowSetting:
	; A valid press of the 'MODE' button has been detected.
	pushbuttons(#4)
	jz setReflowJump
    mov a, Mode_sel ;increment mode
    add a, #0x01
    da a
    mov Mode_sel, a
    ret
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
	
	pushbuttons(#6)
	jz activateOven
	
	ret
;----------------------------------------------------------------------------------------------------------

INCSoakTime:
    mov a, Soak_time
    add a, #0x05
    da a
    mov Soak_time, a
    ljmp setSoak

DECSoakTime:
    mov a, Soak_time
    add a, #0x95
    da a
    mov Soak_time, a
    ljmp setSoak

INCSoakTemp:
    mov a, Soak_temp
    add a, #0x05
    da a
    mov Soak_temp, a
    ljmp setSoak

DECSoakTemp:
    mov a, Soak_temp
    add a, #0x95
    da a
    mov Soak_temp, a
    ljmp setSoak

;-------------------------
INCReflowTime:
    mov a, Reflow_time
    add a, #0x05
    da a
    mov Reflow_time, a
    ljmp setReflow

DECReflowTime:
    mov a, Reflow_time
    add a, #0x95
    da a
    mov Reflow_time, a
    ljmp setReflow

INCReflowTemp:
    mov a, Reflow_temp
    add a, #0x05
    da a
    mov Reflow_temp, a
    ljmp setReflow

DECReflowTemp:
    mov a, Reflow_temp
    add a, #0x95
    da a
    mov Reflow_temp, a
    ljmp setReflow
    END