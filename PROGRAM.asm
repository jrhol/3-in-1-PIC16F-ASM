; operating BUILD CONFIGURATIONS drop down menu in the DEBUG toolbar
; FOR SIMULATIONS with MPLAB SIM: select "Debug" this will switch off delays that take thousands of instructions
; HARDWARE: select "Release" all delays will be on

; Provided code - do not edit  
; This include setups up various configuration bits for the microcontroller
; we are using, and importantly reserves space for variables.
; If you need to use more variables, please place them in VAR.INC, which you
; can find under the "Header Files" folder. The variables listed there will be
; placed in the first memory bank.
; This code has been provided for you to simplify your work, but you should be
; aware that you cannot ignore it.
#include	ECH_1.inc
		

; Place your SUBROUTINE(S) (if any) here ...  
;{ 
ISR	CODE	H'20'
ISR					;ISR called when Button is pressed (INT)
	movlw	d'1'	;Sets Config mode to 1 for each mode
	movwf	ConfigMode	;Each Mode will call specific config mode depending on which mode is currently active
	bcf     INTCON, INTF ; Clear INT flag once intterupt serviced
	retfie	
;} end of your subroutines
	
SampleTime 		;Used for the ADC in the main loop
	movlw d'10'	;Delay of 10ms to allow ADC input to stablise
	call DelWms
return

VariablePWMInit		;Initialisation function called when VariablePWM mode is selected
	
	;clearing Values   
	banksel PIE1
	clrf PIE1
    
	banksel CCP1CON
	clrf CCP1CON
    
	banksel TMR2
	clrf TMR2
	
	banksel LEDs
	clrf LEDs
	
	;Step 1 Set up Timer2 for PWM operation
	BANKSEL PR2			;Select bank 1 and PR2 register
	MOVLW b'00011000'	;Set the PR2 value for 10KHz PWM frequency
						;See formula but this value needs to be 19
	MOVWF PR2
    
	;Step 2 Configure CCP module for PWM
	BANKSEL CCP1CON    ; Select bank 1 and CCP1CON register
	movlw b'00001100'	; 1100<3:0> is PWM active High on all PWM ports, 00<5:4> is our required Duty LSB, 01<7:6> is for setup
	movwf CCP1CON		;Note: Not useing the Duty Cycle LSB here so set to 00 and not changed.

    
	;Step 3
	;Set the initial duty cycle MSB
	BANKSEL CCPR1L     ; Select bank 1 and CCPR1L register
	MOVLW b'00000000'  ; Clearing the Duty here as will be set in the main variable loop function
	MOVWF CCPR1L
	
	
	;Step 4
	;Configure and start Timer 2 for 10KHz
	banksel PIR1
	bcf    PIR1,2	    ;Clearing the TMR2IF interrupt flag
    
	BANKSEL T2CON	    ; Select bank 0 and T2CON register
	MOVLW b'00000101'   ; 1:1 Timer 2 Prescaler
						; 1:4 postscaler
						; Turns T2CON on (Set bit 2)
	MOVWF T2CON
	
	;Enable Steering to P1B
	BANKSEL	PSTRCON
	movlw	b'00010010' ;Enables Sterring to P1B
	movwf	PSTRCON
	
	BANKSEL PIR1 
	btfss   PIR1, 1 ;wait until Timer 2 Overflows before starting
	goto    $-1
    
	;Setting Init incomplete for other two modes
	;This means when the mode changes the other modes know to reinitialise
	movlw d'0'
	movwf	LinearFeedbackInitComplete
	movwf	StrobeInitComplete
	
	;Setting Variable PWM init complete so is only initialised once after mode selected
	movlw d'1'
	movwf VariablePWMInitComplete
return
	
VariablePWM	;Main Variable PWM Function
	    btfss VariablePWMInitComplete,0 ;Check if Initialisation is complete
	    call VariablePWMInit			;Call initialisation if not yet performed
	    
	    btfsc   ConfigMode,0 			;Check if config enabled via ISR
	    call    VariablePWMConfig		;Calls config function if desired
	    
	    movfw   Brightness2				;Moves Brightness2 into a working variable for the loops called LoopCountdown
	    movwf   LoopCountdown
	    
    PWMLoopDown							;PWM 'Count Down loop'
	    movwf SpeedSelectPWM			;User Specified speed for the variable PWM
	    call DelWms						
	    
	    BANKSEL CCPR1L     				; Select bank 1 and CCPR1L register
	    movfw LoopCountdown				;Sets the Working variable LoopCountdown to the duty
	    MOVWF CCPR1L					;Sets the new duty
	    
	    ;loopcountdown initially holds Brightness2
	    ;Compare birghtness 1 with 2 if no match then reduce 2 
		;This repeats over and over until the Brightness 2 matches Brightness1.
		;At this point program leaves PWMLoopDown and now enters PWMLoopUp to count up
	    DECF    LoopCountdown,F		;Decreases LoopCountdown AKA our current duty
	    movfw   Brightness1			
	    xorwf   LoopCountdown,w		;Xors current LoopCountdown with brightness 1
	   
	    btfss   STATUS,Z 		;Checks if Xor was 0 AKA if the LoopCountdown matches Brightnes1
	    goto    PWMLoopDown    ; jump back to the loop label if count is not zero
	
	movfw   Brightness1			;Moves Brightness1 into a working variable for the loops called LoopCountup
	movwf   LoopCountup
	    
	PWMLoopUp
	    movwf SpeedSelectPWM		;User Specified speed for the variable PWM
	    call DelWms
	    
	    BANKSEL CCPR1L    			;Select bank 1 and CCPR1L register
	    movfw LoopCountup			;Sets the Working variable LoopCountup to the initial duty of brightness1
	    MOVWF CCPR1L				;Sets the new duty
	    
	    INCF    LoopCountup,F		;Increases LoopCountup value
	    movfw   Brightness2			;Moves to W reg
	    xorwf   LoopCountup,w		;Checks if the LoopCountup value is the same as Brightness 2, stores output in working
	    
	    btfss   STATUS,Z 		;Checks if Xor was 0
	    goto    PWMLoopUp    ; jump back to the loop label if count is not zero    
		;Completes if value was equal to 0
		;repeats
return

VariablePWMConfig ;Config Function called when ISR for config is enabled
	    
	banksel INTCON		
	bcf     INTCON, GIE  ; Global interrupt Diable while in routine
						;Select 4 needs button on INT pin so disable own ISR
			     
	;Disable PWM   
	banksel PIE1
	clrf PIE1
    
	banksel CCP1CON
	clrf CCP1CON
    
	banksel TMR2
	clrf TMR2
	
	banksel	CCPR1L
	clrf	CCPR1L
	
	banksel	T2CON
	clrf	CCPR1L
			     
	banksel LEDs ;Sets LEDS to Alert that PWM config is enabled
	movlw	b'01000000' 
	movwf	LEDs
	
	call Select4 ;Pick option 1   ;**** NEED TO COMMENT THE DUTY CORRESPONDING TO EACH NUMBER****
	movwf	VariablePWMConfig1
	
	BTFSC	VariablePWMConfig1,0
	movlw	D'5'

	BTFSC	VariablePWMConfig1,1
	movlw	D'10'

	BTFSC	VariablePWMConfig1,2
	movlw	D'15'

	BTFSC	VariablePWMConfig1,3
	movlw	D'20'
	
	movwf	SpeedSelectPWM	;Move chosen value into SpeedSelectPWM
	
	call Select4 ;Pick option 2
	movwf	VariablePWMConfig2
	
	BTFSC	VariablePWMConfig2,0
	movlw	D'1'

	BTFSC	VariablePWMConfig2,1
	movlw	D'2'

	BTFSC	VariablePWMConfig2,2
	movlw	D'4'

	BTFSC	VariablePWMConfig2,3
	movlw	D'8'
	
	movwf	Brightness1			;Move chosen value into Brightness1
	
	call Select4 ;Pick option 3
	movwf	VariablePWMConfig3
	
	BTFSC	VariablePWMConfig3,0
	movlw	D'16'

	BTFSC	VariablePWMConfig3,1
	movlw	D'18'

	BTFSC	VariablePWMConfig3,2
	movlw	D'20'

	BTFSC	VariablePWMConfig3,3
	movlw	D'24'
	
	movwf	Brightness2			;Move chosen value into Brightness2
	
	;Clear LED's from config mode
	banksel LEDs
	clrf LEDs
	
	
	movlw	d'0'
	movwf	ConfigMode	;Setting ConfigMode to 0 indicating configuration complete
	movwf	VariablePWMInitComplete	;Sets VariablePWMInitComplete to '0', telling the function initialisation needs to be done again with changes
	
	banksel INTCON
	bcf	INTCON, INTF		;Clear the INT flag caused by using Select4
	bsf     INTCON, GIE  	;Global interrupt renable
return
	
StrobeInit ;Initialisation function for the Strobe Function
	;Disables PWM   
	banksel PIE1
	clrf PIE1
    
	banksel CCP1CON
	clrf CCP1CON
    
	banksel TMR2
	clrf TMR2
	
	banksel	CCPR1L
	clrf	CCPR1L
	
	banksel	T2CON
	clrf	CCPR1L
	
	;Setting Init incomplete for other two modes
	movlw d'0'
	movwf	LinearFeedbackInitComplete
	movwf	VariablePWMInitComplete
		
	;Setting Side to side strobe init as complete
	movlw d'1'
	movwf StrobeInitComplete
return

;Subroutine Used to check if mode has changed while side to side stobe is used
ADCCheck ;Checking Here if mode has changed in the middle of the strobe
	    bsf ADCON0, GO_DONE ;Start Aquestision
	    btfsc ADCON0, GO_DONE ;Wait until aquesition is done
	    goto $-1
	    movfw ADRESH		;Move the 8 MSB (left justified) to working
	    andlw   B'11000000'	; mask unused bits
	    xorlw   d'128'		;Xor the modeselect with 128 (Expected value of ADC for Mode2 after masking)
	    btfss STATUS, Z		;Checks if the outcome was 0, AKA values match
	    goto MLoop	;If outcome isn't 0 check next possible value
return
	
SideToSideStrobe	;Main function for side to side strobe
	    btfss StrobeInitComplete,0	;Check if Initialisation is complete
	    call StrobeInit		;Calles initialisation if not complete
	    
	    bcf	  STATUS,C	
	
	    BTFSS direction_select, 0  ; Check the direction select bit 
	    GOTO ForwardandBack  ;If its 0 do forward and back 
	    GOTO ForwardOnly  	;If its 1 do forward only
	    
    ForwardandBack ;Forward and back loop   	    
	    btfss current_direction,0 ;Check if where in the forward/back cycle the stobe was
	    goto loop1	
	    goto loop2

    loop1
    
	    decf  count, F ; decrement count and skip next instruction if count = 0
	    btfsc   STATUS,Z ;skip if Count is not 0
	    goto    loop1end   ; jump back to the loop label if count is not zero
	    
	    MOVFW led_pattern ; Get the current LED pattern
	    MOVWF PORTD  ; Output the LED pattern to Port D
	    
	    ;Delay Here based user selected option
	    movfw speedSelect
	    call DelWms 
	    
	    RLF led_pattern, F  ; Shift the LED pattern left
	    bcf	  STATUS,C	;Clear the carry bit
	    
	    movlw   d'0'	;Sets the Current Direction bit flag
	    movwf   current_direction

	    btfsc   ConfigMode,0		;Check if config enabled each loop
	    call    SideToSideStrobeConfig	;Calls config if ISR has set the config bit
	    
	    ;Checking Here if mode has changed in the middle of the strobe
	    Call ADCCheck
	    
	    

	    goto    loop1    ; jump back to the loop label if count is not zero
	    
loop1end
	    ;Restarts count for the reverse motion
	    movlw   d'8'       ; load the value 8 into the W register
	    movwf   count   ; store the value 8 into a variable named count 
	    MOVLW	    B'10000000'  ; Initialize the first LED to on
	    MOVWF	    led_pattern
	    
    loop2    
	    decf  count, F ; decrement count and skip next instruction if count = 0
	    btfsc   STATUS,Z ;skip if Count is not 0
	    goto    loop2end   ; jump back to the loop label if count is not zero
	    
	    MOVFW led_pattern ; Get the current LED pattern
	    MOVWF PORTD  ; Output the LED pattern to Port D
	    
	    ;Delay Here based on speed
	    movfw speedSelect
	    call DelWms 
	    
	    RRF led_pattern, F  ; Shift the LED pattern left
	    bcf	  STATUS,C	;Clear the Carry bit
	    
	    movlw   d'1'	;Sets the current direction bit flag
	    movwf   current_direction	

	    btfsc   ConfigMode,0		;Check if config enabled each loop
	    call    SideToSideStrobeConfig	;Calls config if ISR has set the config bit
	    
	    ;Checking Here if mode has changed in the middle of the strobe
	    Call ADCCheck
	    
	    goto    loop2    ; jump back to the loop label if count is not zero
	    
	loop2end
	    
	;Resetting the LED pattern once a full forward and back sequence is complete
	banksel	    led_pattern
	MOVLW	    B'00000001'  ; Initialize the first LED to on
	MOVWF	    led_pattern
	bcf	    STATUS,C		;Clear the Carry Status Bit
	;Resetting the current direction and count
	movlw	    d'0'
	movwf	    current_direction
	movlw	    d'8'
	movwf	    count
	goto MLoop			;Once the forwards and back is complete goto main to check if the operation mode has changed
	
    ForwardOnly	;Forward ONLY loop
	loop3
	    MOVFW led_pattern ; Get the current LED pattern
	    MOVWF PORTD  ; Output the LED pattern to Port D
	    
	    ;Delay Here based on speed
	    movfw speedSelect
	    call DelWms 
	    
	    RLF led_pattern, F  ; Shift the LED pattern left
	    
	    btfsc   ConfigMode,0		;Check if config enabled each loop
	    call    SideToSideStrobeConfig	;Calls config if ISR has set the config bit
	    
	    ;Checking Here if mode has changed in the middle of the strobe
	    Call ADCCheck
	    
	    decfsz  count, F ; decrement count and skip next instruction if count = 0
	    goto    loop3    ; jump back to the loop label if count is not zero
	;Resetting the LED pattern once the forward only sequence is complete
	banksel	    led_pattern
	MOVLW	    B'00000001'  ; Initialize the first LED to on
	MOVWF	    led_pattern
	bcf	    STATUS,C	
	movlw   8       ; load the value 8 into the W register
	movwf   count   ; store the value 8 into a variable named count
	goto MLoop    	;Once the forwards only motion is complete goto main to check if the operation mode has changed
    return
	
SideToSideStrobeConfig	;Configuration Options for the Side to Side strobe
	banksel INTCON
	bcf     INTCON, GIE  ; Global interrupt Diable while in routine
						;Select 4 needs button on INT pin so disable own ISR
	banksel LEDs ;Sets LEDS for config mode
	movlw b'10000000'
	movwf LEDs
	
	call Select4 ;Pick option 1
	movwf	SidetoSideConfig1
	BTFSC	SidetoSideConfig1,0
	movlw	D'10'

	BTFSC	SidetoSideConfig1,1
	movlw	D'50'

	BTFSC	SidetoSideConfig1,2
	movlw	D'100'

	BTFSC	SidetoSideConfig1,3
	movlw	D'255'
	
	movwf	speedSelect
    
	call Select4 ;Pick option 2
	movwf	SidetoSideConfig2
	BTFSC	SidetoSideConfig2,0
	movlw	D'0'

	BTFSC	SidetoSideConfig2,1
	movlw	D'0'

	BTFSC	SidetoSideConfig2,2
	movlw	D'1'

	BTFSC	SidetoSideConfig2,3
	movlw	D'1'
	
	movwf direction_select
	;Direction_select 0 Forwards (0->7) and Back (7->0) 
	;Direction_select 1 goes to 0-7 before resetting back at 0
	
	banksel ConfigMode	;Sets Config mode to 0 indicating config is finished
	movlw	d'0'
	movwf	ConfigMode
	
	banksel INTCON
	bcf	INTCON, INTF		;Clear the INT flag caused by using Select4
	bsf     INTCON, GIE		; Global interrupt renable
return

	
xor ;Function used by the main Linear feedback register function
	movfw	poly		;Xor the LFSR_reg with the polynomial
	xorwf	LFSR_reg,f	;Stores the outcome of the XOR in the LFSR_reg variable
return	
	
LinearFeedbackRegisterInit	;Initialises the LFSR
	;Disable PWM
	banksel PIE1
	clrf PIE1
    
	banksel CCP1CON
	clrf CCP1CON
    
	banksel TMR2
	clrf TMR2
	
	banksel	CCPR1L
	clrf	CCPR1L
	
	banksel	T2CON
	clrf	CCPR1L
	
	
	;Setting Init incomplete for other two modes
	movlw d'0'
	movwf	VariablePWMInitComplete
	movwf	StrobeInitComplete
	
	;Setting LinearFeedbackInitComplete complete
	movlw d'1'
	movwf LinearFeedbackInitComplete

	return

LinearFeedbackRegister ;Main function for linear feedback register
	btfss LinearFeedbackInitComplete,0 ;Check if Initialisation is complete
	call LinearFeedbackRegisterInit		;if not call initialisation function
	
	btfsc   ConfigMode,0				;Checks to see if the config is active
	call    LinearFeedbackRegisterConfig
	
	movfw	LFSR_reg	;Moves the initial LFSR value to the LEDs
	movwf	PORTD
	movwf	LFSR_regtemp
	
	movfw	LSFR_time	;Calls delay for the user specified time
	call	DelWms

	bcf	STATUS, C
	rrf	LFSR_reg,f	;Shift the LFSR value to the right once
	
	btfsc	LFSR_regtemp,0	;Tests the LSB of the LFSR, if it is 1 then calls the XOR function
	call	xor	

	return

	
LinearFeedbackRegisterConfig	;COnfiguration mode for the LFSR utility
	banksel INTCON
	bcf     INTCON, GIE  ; Global interrupt Diable while in routine
						;Select 4 needs button on INT pin so disable own ISR
	banksel LEDs ;Sets LEDs for config mode
	movlw b'11000000'
	movwf LEDs
	
	call Select4 ;Pick option 1 	;Aka time between new LFSR values
	movwf	LinearFeedbackConfig1
	BTFSC	LinearFeedbackConfig1,0
	movlw	D'10'

	BTFSC	LinearFeedbackConfig1,1
	movlw	D'50'

	BTFSC	LinearFeedbackConfig1,2
	movlw	D'100'

	BTFSC	LinearFeedbackConfig1,3
	movlw	D'255'
	
	movwf	LSFR_time
	
	
	movlw	d'0'		;Sets config mode to 0 indicating config complete
	movwf	ConfigMode
	
	banksel LEDs		;Clears the config LED's
	clrf	LEDs
	
	banksel INTCON
	bcf	INTCON, INTF	;Clear the INT flag caused by using Select4
	bsf     INTCON, GIE  ; Global interrupt enable
	return
	
; Provided code - do not edit  
Main	nop
#include ECH_INIT.inc

; Place your INITIALISATION code (if any) here ...   
;{ ***		***************************************************
; e.g.,		movwf	Ctr1 ; etc

;Set LED's as output
banksel TRISD
movlw	b'00000000'
movwf   TRISD ; Set LEDS as output
		
;Enable ISR on INT pin for Config mode activation
banksel TRISB
bsf     TRISB, RB0 ; Set INT pin as input
	
banksel OPTION_REG
bsf     OPTION_REG, INTEDG ; Enable interrupt on falling edge

banksel INTCON
bsf     INTCON, GIE  ; Global interrupt enable
bsf     INTCON, INTE ; Enable external interrupt
		
;ADC SETUP		
banksel ADCON1 ;Left Justify, Reference as VSS and GND
;Done this way as select 4 uses same justification so saves having to do as many periperhal changes between modes
movlw b'00000000'
movwf ADCON1
	
banksel TRISA	;Set RA0 to input
bsf TRISA, 0
	
banksel ANSEL ;Set RA0 to analogue	
bsf ANSEL, 0

banksel ADCON0 
movlw b'01000001' ;ADC Conversion Clock set bits to internal clock, Set input to AD1, Enable ADC. Clear Go/done bit
movwf ADCON0	  ;ADC <01> is for Internal Clock
	
Call SampleTime		;Call sample time to allow the ADC input to stabalise
				
				
;Setting Up Initial Variables for Each of the modes
	;Variable PWM variabels
	;Setting initial Values for user choices
	movlw	d'5'
	movwf Brightness1
	
	movlw	d'30'
	movwf Brightness2
	
	movlw	d'50'
	movwf	SpeedSelectPWM
	
	;Side to Side Strobe Variables
	;Setting Initial Values for User config
	movlw	D'255'
	movwf	speedSelect
	;Setting Initial Direction
	movlw	D'0'
	movwf 	direction_select
	
	banksel led_pattern
	MOVLW B'00000001'  ; Initialize the first LED to on
	MOVWF led_pattern
	
	movlw d'8'
	movwf	count
				
	;LFSR Variables		
	;Initial Value of the time between LFSR values 
	movlw 	d'255'
	movwf	LSFR_time
	
	;Inital Value of the LFSR
	MOVLW 0x01
	MOVWF LFSR_reg
    
	;Sets the Value of the Polynomial
	MOVLW 0xB8 	;Represents the polynomial '10111000' Aka X^8 + x^6 + x^5 + x^4 +1 which is maximal period 
	MOVWF poly
	
	
		
;} 
; end of your initialisation

MLoop	nop

; place your superloop code here ...  
;{
;Read ADC
bsf ADCON0, GO_DONE ;Start Aquestision
btfsc ADCON0, GO_DONE ;Wait until aquesition is done
goto $-1
movfw ADRESH		;Move the 8 MSB (left justified) to working
		
andlw	B'11000000'	; mask unused bits
movwf ModeSelect	;Move masked value into variable
		
    ; Compare the value to 0
    movfw ModeSelect
    xorlw d'0'		;Xor the modeselect with 0
    btfss STATUS, Z	;Checks if the outcome was 0
    goto check_two	;If outcome isn't 0 check next possible value
        
    ; Call the mode one subroutine
    call VariablePWM	;Call first mode 
    goto MLoop			;When returning from from call, start main again
        
    check_two
    ; Compare the value to 2
    movfw ModeSelect
    xorlw d'128'		;Xor the modeselect with 128 (Expected value of ADC for Mode2 after masking)
    btfss STATUS, Z		;Checks if the outcome was 0, AKA values match
    goto check_three	;If outcome isn't 0 check next possible value
        
    ; Call the mode two subroutine
    call SideToSideStrobe
    goto MLoop			;When returning from from call, start main again
        
    check_three
    ; Compare the value to 3
    movfw ModeSelect
    xorlw d'192'		;Xor the modeselect with 192 (Expected value of ADC for Mode3 after masking)
    btfss STATUS, Z		;Checks if the outcome was 0, AKA values match
    goto MLoop			;If outcome isn't 0 goes to start of main
        
    ; Call the mode three subroutine
    call LinearFeedbackRegister		

;}	
; end of your superloop code

    goto	MLoop

end
