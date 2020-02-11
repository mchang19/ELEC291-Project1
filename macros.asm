
;------------Retreive temp readings from adc---------------------;

Read_Temp_Channel mac	
	mov b, #%0
	lcall ?Read_Temp_Channel
endmac

?Read_Temp_Channel:
	clr CE_ADC
	mov R0, #00000001B
	lcall DO_SPI_G
	
	;the following set of instructions replace 	'mov R0, #10000000B' 
	;since we could be dealing with non-zero channels
	mov a, b ;specify channel 
	swap a
	anl a, #0F0H  ;AND with upper 4 MSB's
	setb acc.7    ;MSB set 
	mov R0, a
	
	;back to familiar code
	lcall DO_SPI_G
	mov a, R1
	anl a, #00000011B
	mov Result+1, a
	mov R0, #55H
	lcall DO_SPI_G
	mov Result, R1
	setb CE_ADC
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
	sjmp button0
	
checkbutton1:
	cjne R2, #1, checkbutton2
	sjmp button1
	
checkbutton2:
	cjne R2, #2, checkbutton3
	sjmp button2
	
checkbutton3:
	cjne R2, #3, checkbutton4
	sjmp button3
	
checkbutton4:
	cjne R2, #4, checkbutton5
	sjmp button4
	
	
checkbutton5:
	cjne R2, #5, checkbutton6
	sjmp button5
	
checkbutton6:
	cjne R2, #6, checkbutton_mode
	sjmp button6
	
checkbutton_mode:
	cjne R2, #7, done 
	sjmp pushed_mode_button

done:
	clr a
	ret
	
button0:
	jb PB0 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB0, not_pushed
	jnb PB0, $
	mov a, #1
	ret
	
button1:
	jb PB1 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB1, not_pushed
	jnb PB1, $
	mov a, #1
	ret
	
button2:
	jb PB2 , not_pushed
	Wait_Milli_Seconds(#50)	
	jb PB2, not_pushed
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
	
pushed_button_mode:
	jb MODE_BUTTON , not_pushed
	Wait_Milli_Seconds(#50)	
	jb MODE_BUTTON, not_pushed
	jnb MODE_BUTTON, $
	mov a, #1
	ret
	
not_pushed:
	mov a, #0
	ret
		