; list names of your variables one in a row below
; each of them will be allocated a GPR
; and can be referenced by name in your code
; (the assembler will place actual address automatically)
; examples:
;Ctr1	RES	1	; your vaiable, e.g., for counter 1
;Ctr2	RES	1
Temp	Res	1	; this is used by the examples
A		Res	1
Bop		Res	1

ConfigMode	Res	1
ModeSelect	Res	1
		
VariablePWMInitComplete	    Res 1		
VariablePWMConfig1  Res	1
SpeedSelectPWM	    Res	1
VariablePWMConfig2  Res	1
Brightness1	    Res 1
VariablePWMConfig3  Res	1
Brightness2	    Res 1
LoopCountdown	    Res 1
LoopCountup	    Res 1

StrobeInitComplete  Res 1
count		    Res 1		    
led_pattern	    Res 1
SidetoSideConfig1   Res	1
speedSelect	    Res 1
SidetoSideConfig2   Res	1
direction_select    Res 1
current_direction   Res 1
		
LinearFeedbackInitComplete Res 1 
LFSR_reg	    Res 1
poly		    Res 1
LinearFeedbackConfig1	Res 1
LSFR_time   Res	1
LFSR_regtemp	Res 1
   


