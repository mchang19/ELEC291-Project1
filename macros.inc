
;------------Retreive temp readings from adc---------------------;
read_temperature:
	; Take 256 (4^4) consecutive measurements of ADC0 channel 0 at about 10 us intervals and accumulate in x
	Load_x(0)
    mov x+0, AD0DAT0
	mov R7, #255
    lcall Wait10us
accumulate_loop:
    mov y+0, AD0DAT0
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
	Load_Y(410) ; Vref is 3.3V
	lcall mul32
	Load_Y(5) ; 2^12-1
	lcall div32
	Load_Y(22)
	lcall add32
	Load_Y(273)
	lcall sub32
ret

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
		


;-------checking whether buttons are pushed------------;

pushbuttons mac
	push AR2 
	mov R2, #%0
	lcall ?pushbuttons
	pop AR2
endmac
?pushbuttons:
	cjne R2, #0, checkbutton1
	ljmp button0
checkbutton1:
	cjne R2, #1, checkbutton2
	ljmp button1
checkbutton2:
	cjne R2, #2, checkbutton3
	ljmp button2	
checkbutton3:
	cjne R2, #3, checkbutton4
	ljmp button3
checkbutton4:
	cjne R2, #4, checkbutton5
	ljmp button4	
checkbutton5:
	cjne R2, #5, checkbutton6
	ljmp button5
checkbutton6:
	cjne R2, #6, checkbutton_mode
	ljmp button6
checkbutton_mode:
	cjne R2, #7, done 
	ljmp pushed_mode_button
done:
	clr a
	ret

GOTOnot_pushed:
	ljmp not_pushed
	
button0:
	jb PB0 , GOTOnot_pushed
	Wait_Milli_Seconds(#50)	
	jb PB0, GOTOnot_pushed
	jnb PB0, $
	mov a, #1
	ret	
button1:
	jb PB1 , GOTOnot_pushed
	Wait_Milli_Seconds(#50)	
	jb PB1, GOTOnot_pushed
	jnb PB1, $
	mov a, #1
	ret	
button2:
	jb PB2 , GOTOnot_pushed
	Wait_Milli_Seconds(#50)	
	jb PB2, GOTOnot_pushed
	jnb PB2, $
	mov a, #1
	ret	
button3:
	jb PB3 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB3, not_pushed
	jnb PB3, $
	mov a, #1
	ret
button4:
	jb PB4 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB4, not_pushed
	jnb PB4, $
	mov a, #1
	ret	
button5:
	jb PB5 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB5, not_pushed
	jnb PB5, $
	mov a, #1
	ret
button6:
	jb PB6 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB6, not_pushed
	jnb PB6, $
	mov a, #1
	ret
pushed_mode_button:
	jb MODE_BUTTON , not_pushed
	Wait_Milli_Seconds(#50)	
	jb MODE_BUTTON, not_pushed
	jnb MODE_BUTTON, $
	mov a, #1
	ret
not_pushed:
	mov a, #0
	ret
		