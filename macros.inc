
SendTemp:
	mov dptr, #HexAscii 
	
	mov a, bcd+1
	swap a
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar
	mov a, bcd+1
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar

	mov a, bcd+0
	swap a
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar
	mov a, bcd+0
	anl a, #0xf
	movc a, @a+dptr
	lcall putchar
	
	mov a, #'\r'
	lcall putchar
	mov a, #'\n'
	lcall putchar	
	ret
	
	
;--------------------------------------------
_convert_time mac
	mov x+0, BCD_counter+0
	mov x+1, BCD_counter+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
endmac
 
_convert_to_bcd mac
	mov x+0, %0
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
endmac		


read_temp mac
	lcall readADC
	lcall calcTemp
	mov x+0, temp+0
	mov x+1, temp+1
	mov x+2, temp+2
	mov x+3, temp+3	
	lcall hex2bcd

endmac


;-----------------------------------

	
readADC:
	mov a, AD0DAT3
	mov thermo_temp, a
	;theoretically we would also read from the LM335 lmaooOOOO
	mov a, AD0DAT1
	mov room_temp, a
	
	ret 

calcTemp:
	mov x+0, thermo_temp
	mov x+1, #0
	mov x+2, #0
	mov x+3, #0
	
	mov y+0, room_temp
	mov y+1, #0
	mov y+2, #0
	mov y+3, #0
	

	lcall sub32
	Load_y(330)
	lcall mul32 
	Load_y(255)
	lcall div32
	;Load_y(120)
	;lcall div32
	Load_y(273)
	lcall sub32

	
	mov temp+0, x+0
	mov temp+1, x+1
	mov temp+2, x+2
	mov temp+3, x+3
	
	ret


;---------------------------------------
;------------Retreive temp readings from adc---------------------;
read_temperature:
	;clr EA
	; Take 256 (4^4) consecutive measurements of ADC0 channel 0 at about 10 us intervals and accumulate in x
	Load_x(0)
    mov x+0, AD0DAT3
	mov R7, #255
    lcall Wait10us
accumulate_loop:
    mov y+0, AD0DAT3
    mov y+1, #0
    mov y+2, #0
    mov y+3, #0
    lcall add32
    lcall Wait10us
	djnz R7, accumulate_loop
	
	; Now divide by 16 (2^4)
	Load_Y(16)
	lcall div32
	; x has now the 12-bit representation of the temperature
	
	; Convert to temperature (C)
	Load_Y(33000) ; Vref is 3.3V
	lcall mul32
	Load_y(((1<<12)-1)) ; 2^12-1
	lcall div32
	Load_y(120)
	lcall mul32
	Load_y(100)
	lcall div32
	Load_y(2200)
	lcall add32
	
	mov temp, x+0			;store result of thermocouple in temp
	mov temp+1, x+1
	mov temp+2, x+2
	mov temp+3, x+3
	;setb EA
	
ret		   

END