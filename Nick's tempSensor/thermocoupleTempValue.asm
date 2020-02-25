$MOD9351

  TIMER0_RELOAD_L DATA 0xf2
  TIMER1_RELOAD_L DATA 0xf3
  TIMER0_RELOAD_H DATA 0xf4
  TIMER1_RELOAD_H DATA 0xf5

  XTAL EQU 7373000
  BAUD EQU 115200
  BRVAL EQU ((XTAL/BAUD)-16)

  ;---------------;
  ;  Constants    ;
  ;---------------;
  CLK           EQU 14746000 ; Microcontroller system crystal frequency in Hz
  TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
  TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
  TIMER1_RATE   EQU 2000     ; 1000Hz, for a timer tick of 1ms
  TIMER1_RELOAD EQU ((65536-(CLK/TIMER1_RATE)))


  ; Reset vector
  org 0x0000
    ljmp main

  ; External interrupt 0 vector (not used in this code)
  org 0x0003
    reti

  ; Timer/Counter 0 overflow interrupt vector
  org 0x000B
    reti

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
    reti  
    
  ; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
    Reflow_time:	ds 2;
    Reflow_temp:	ds 2;
    Soak_time:		ds 2;
    Soak_temp:		ds 2;
    temp:			ds 1;

    Result: 		ds 2;
    x:				ds 4;
    y:				ds 4;
    bcd:			ds 5;

bseg
	mf:				dbit 1

cseg
    LCD_RS equ P0.5
    LCD_RW equ P0.6
    LCD_E  equ P0.7
    LCD_D4 equ P3.0
    LCD_D5 equ P3.1
    LCD_D6 equ P1.2
    LCD_D7 equ P1.3

$NOLIST
$include(LCD_4bit_LPC9351.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$include(macros.inc)
$LIST

sendTemp:
	Send_BCD2(bcd+1)
	Send_BCD(bcd)
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar
ret

HexAscii: db '0123456789ABCDEF'

putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret
	
SendString:
    clr a
    movc a, @a+dptr
    jz SendString_L1
    lcall putchar
    inc dptr
    sjmp SendString  
SendString_L1:
	ret

Wait10us:
    mov R0, #18
    djnz R0, $ ; 2 machine cycles-> 2*0.27126us*18=10us
	ret

displayTemp:
    lcall read_temperature
    lcall hex2bcd
    lcall shiftBCDdown
    Set_Cursor(1,11)
    lcall LCD_3BCD			;display temp
    WriteData(#0b11011111)
    WriteData(#'C')
    Wait_Milli_Seconds(#20)
    clr mf
	lcall sendTemp
	clr mf
    lcall bcd2hex
    clr mf
    mov a, Soak_temp
    mov y+0, a    ;hex for 140 degrees (using 2 bits) supposed to be Soak_temp 
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall x_gt_y
    jnb mf, showLOW  ;if current temp > soak_temp, mf = 1, if mf = 1, then go to state 2

showHIGH:
    Set_Cursor(2,1)
    WriteData(#'H')
    WriteData(#'I')
ret

showLOW:
    Set_Cursor(2,1)
    WriteData(#'L')
    WriteData(#'O')
ret

; Display a 3-digit BCD number in the LCD
LCD_3BCD:
	mov a, bcd+1
	anl a, #0x0f
	orl a, #'0'
	lcall ?WriteData
	mov a, bcd+0
	swap a
	anl a, #0x0f
	orl a, #'0'
	lcall ?WriteData
	mov a, bcd+0
	anl a, #0x0f
	orl a, #'0'
	lcall ?WriteData
	ret

shiftBCDdown:
	mov bcd+0, bcd+1
	mov bcd+1, bcd+2
	mov bcd+2, bcd+3
	mov bcd+3, bcd+4
	mov bcd+4, #0
ret

InitSerialPort:
	mov	BRGCON,#0x00
	mov	BRGR1,#high(BRVAL)
	mov	BRGR0,#low(BRVAL)
	mov	BRGCON,#0x03 ; Turn-on the baud rate generator
	mov	SCON,#0x52 ; Serial port in mode 1, ren, txrdy, rxempty
	mov	P1M1,#0x00 ; Enable pins RxD and TXD
	mov	P1M2,#0x00 ; Enable pins RxD and TXD
	ret

InitADC0:
	; ADC0_0 is connected to P1.7
	; ADC0_1 is connected to P0.0

    ; Configure pins P1.7, P0.0  as inputs
    ;orl P0M1, #00000001b
    ;anl P0M2, #11111110b
    
    orl P1M1, #10000000b
    anl P1M2, #01111111b
	orl P2M1, #00000011b
    anl P2M2, #01111100b
    
	; Setup ADC0
	setb BURST0 ; Autoscan continuos conversion mode
	mov	ADMODB,#0x20 ;ADC0 clock is 7.3728MHz/2
	mov	ADINS,#0x0E ; Select two channels of ADC0 for conversion
	mov	ADCON0,#0x05 ; Enable the converter and start immediately
	; Wait for first conversion to complete
InitADC0_L1:
    mov	a,ADCON0
    jnb	acc.3,InitADC0_L1
    ret

main:
    mov SP, #0x7F   
    mov P0M1, #00H
    mov P0M2, #00H
    mov P1M1, #00H
    mov P1M2, #00H ; WARNING: P1.2 and P1.3 need 1kohm pull-up resistors!
    mov P2M1, #00H
    mov P2M2, #00H
    mov P3M1, #00H
    mov P3M2, #00H
    lcall InitSerialPort
    lcall InitADC0
    lcall LCD_4BIT
    mov Soak_temp, #140
    
forever:
    lcall displayTemp
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
    ljmp forever

END