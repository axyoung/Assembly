;***********************************************************
;*
;*	Alex_Young_and_Steven_Bui_Lab8_sourcecode.asm
;*
;*	This program implements a morse code translator
;*  that writes characters from the alphabet onto
;*  the LCD and then turns them into morse code.
;*
;***********************************************************
;*
;*	 Author: Alex Young and Steven Bui
;*	   Date: 3/8/21
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	waitcnt = r23			; Wait Loop Counter
.def	ilcnt = r24				; Inner Loop Counter
.def	olcnt = r25				; Outer Loop Counter
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	length = r5				; Holds the current length of the word in LCD
.def	toggle = r6				; Hods the current state of the timer1 overflow

.equ	WTime = 15				; Time to wait in wait loop for 150 ms

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MorseLED = (1<<5|1<<6|1<<7|1<<4)	; Control Lights
.equ	MorseReset = (1<<4)					; Reset Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; place instructions in interrupt vectors here, if needed
.org	$001C					; Timer/Counter1 Overflow
		rcall	TIMER_OVERFLOW	; Overflow triggers...
		reti					; Return from interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr		; Load SPL with low byte of RAMEND
		ldi		mpr, high(RAMEND)
		out		SPH, mpr		; Load SPH with high byte of RAMEND

		; Configure I/O ports
		; Initialize Port B for output
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low

		; Initialize Port D for input
		ldi		mpr, 0b0010_1110	; Set Port D Data Direction Register
		out		DDRD, mpr			; for input
		ldi		mpr, $FF			; Initialize Port D Data Register
		out		PORTD, mpr			; so all Port D inputs are Tri-State

		; Initialize LCD Display
		rcall	LCDInit			; Initialize LCD peripheral interface
		
		ldi		mpr, 0b0000_0100	; Clock select with normal mode and prescaler 256
		out		TCCR1B, mpr			; Load into Timer1
		ldi     r16, 0b000_0100     ; enable Timer/Counter 1
        out     TIMSK, r16          ; overflow interrupt
		
        ldi     mpr, high(3906)		; User 3906 as the value for TCNT1 because 3906 * 256 is 1 second
        out     TCNT1H, mpr         ; high byte of TCNT1
        ldi     mpr, low(3906)		; write the low byte
        out     TCNT1L, mpr         ; to TCNT1

		mov		toggle, zero		; Set the toggle to 0
		sei

		rcall	WELCOME_LCD			; Set up the welcome LCD screen
		rcall	WAIT_LCD			; Wait for user to press PD0
		rcall	SECOND_INIT_LCD		; Initialize the LCD and program memory to display Enter word: A

		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		
		rcall	SET_LCD						; Write to the LCD
		in		mpr, PIND					; Load in the buttons to check with polling
		andi	mpr, (1<<0|1<<4|1<<6|1<<7)	; Looking for buttons 0 4 6 7
		cpi		mpr, (1<<4|1<<6|1<<7)		; If the button is not 0
		brne	NEXT_1						; Check next button
		rcall	CONFIRM_CHAR				; Otherwise, confirm the character
		ldi		waitcnt, WTime				; Wait for .15 second
		rcall	WAIT_LOOP					; Call wait function
		rjmp	MAIN						; Loop back to main

NEXT_1:
		cpi		mpr, (1<<0|1<<6|1<<7)		; If the button is not 4
		brne	NEXT_2						; Check the next button
		rcall	BEGIN_TRANSMISSION			; Otherwise, start morse transmission
		ldi		waitcnt, WTime				; Wait for .15 second
		rcall	WAIT_LOOP					; Call wait function
		rjmp	MAIN						; Loop back to main

NEXT_2:
		cpi		mpr, (1<<0|1<<4|1<<7)		; If the button is not 6
		brne	NEXT_3						; Check the next button
		rcall	ITERATE_FORWARD				; Otherwise, iterate the character forward A->B
		ldi		waitcnt, WTime				; Wait for .15 second
		rcall	WAIT_LOOP					; Call wait function
		rjmp	MAIN						; Loop back to main

NEXT_3:
		cpi		mpr, (1<<0|1<<4|1<<6)		; If the button is not 7
		brne	MAIN						; loop back to main
		rcall	ITERATE_BACKWARD			; Otherwise, iterate the character backwards A->Z
		ldi		waitcnt, WTime				; Wait for .15 second
		rcall	WAIT_LOOP					; Call wait function
		rjmp	MAIN
											; jump back to main and create an infinite
											; while loop.  Generally, every main program is an
											; infinite while loop, never let the main program
											; just run off

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: WELCOME_LCD
; Desc: Display the welcome lcd strings
;-----------------------------------------------------------
WELCOME_LCD:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		ldi		ZL, low(STRING1_BEG<<1)		; Load the low byte of program memory address to ZL register
		ldi		ZH, high(STRING1_BEG<<1)	; Load the high byte of program memory address to ZH register
		ldi		YL, $00						; Load the low byte of the data memory address to YL register 
		ldi		YH, $01						; Load the high byte of the data memory address to YH register 
		
DO1:										; Loop through characters in string 1
		lpm		mpr, Z+						; Load memory from Z register to mpr and increment Z
		st		Y+, mpr						; Store character from memory to character destination and increment Y
		cpi		ZL, low(STRING1_END<<1)		; Stop looping when reach the program memory address after last character
		brne	DO1							; Loop through the do statement otherwise
		
		ldi		ZL, low(STRING2_BEG<<1)		; Load the low byte of program memory address to ZL register
		ldi		ZH, high(STRING2_BEG<<1)	; Load the high byte of program memory address to ZH register
		ldi		YL, $10						; Load the low byte of the data memory address to YL register 
		ldi		YH, $01						; Load the high byte of the data memory address to YH register 
		
DO2:										; Loop through characters in string 2
		lpm		mpr, Z+						; Load memory from Z register to mpr and increment Z
		st		Y+, mpr						; Store character from memory to character destination and increment Y
		cpi		ZL, low(STRING2_END<<1)		; Stop looping when reach the program memory address after last character
		brne	DO2							; Loop through the do statement otherwise
		
		rcall	LCDWrite					; Print out the welcome message

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: WAIT_LCD
; Desc: Can't get into next screen until press button
;-----------------------------------------------------------
WAIT_LCD:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		in		mpr, PIND					; Load in the buttons
		andi	mpr, (1<<0|1<<4|1<<6|1<<7)	; Checking for buttons 0 4 6 7
		cpi		mpr, (1<<4|1<<6|1<<7)		; If button is not 0
		brne	WAIT_LCD_LOOP				; Loop until button press

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET
		
WAIT_LCD_LOOP:
		rjmp	WAIT_LCD		; Loop back to the start

;-----------------------------------------------------------
; Func: SET_LCD
; Desc: Resets to initial state of Enter word: A
;-----------------------------------------------------------
SET_LCD:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		rcall	LCDClr			; Clear the LCD values in memory

		ldi		ZL, low(STRING3_BEG<<1)		; Load the low byte of program memory address to ZL register
		ldi		ZH, high(STRING3_BEG<<1)	; Load the high byte of program memory address to ZH register
		ldi		YL, $00						; Load the low byte of the data memory address to YL register 
		ldi		YH, $01						; Load the high byte of the data memory address to YH register 
		
DO3:										; Loop through characters in string 1
		lpm		mpr, Z+						; Load memory from Z register to mpr and increment Z
		st		Y+, mpr						; Store character from memory to character destination and increment Y
		cpi		ZL, low(STRING3_END<<1)		; Stop looping when reach the program memory address after last character
		brne	DO3							; Loop through the do statement otherwise
		
		rcall	InitLCD2					; Set the LCD values in program memory with new values
		rcall	LCDWrite					; Write out the LCD to the screen

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: TIMER_OVERFLOW
; Desc: This subroutine will toggle the toggle register
;		for every overflow of timer1
;-----------------------------------------------------------
TIMER_OVERFLOW:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		ldi		mpr, 1			; Load mpr with 1 an XOR with toggle
		eor		toggle, mpr		; to toggle toggle
		
        ldi     mpr, high(3906)	; Use 3906 as value with prescale of 256 for 1 second
        out     TCNT1H, mpr     ; high byte of TCNT1
        ldi     mpr, low(3906)  ; write the low byte
        out     TCNT1L, mpr     ;

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: NEXT_CHAR
; Desc: Will check the state of toggle using polling in
;		order to wait an extra 2 seconds
;-----------------------------------------------------------
NEXT_CHAR:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
CHECK_TOGGLE7:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 0			; Compare it to 0
		brne	CHECK_TOGGLE7	; Loop until toggle is 0
CHECK_TOGGLE8:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 1			; Compare it to 1
		brne	CHECK_TOGGLE8	; Loop until toggle is 1

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: DOT
; Desc: Will check the state of toggle using polling in
;		order to display LEDs for 1 second and wait 1 second
;-----------------------------------------------------------
DOT:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
CHECK_TOGGLE:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 0			; Compare it to 0
		brne	CHECK_TOGGLE	; Loop until toggle is 0
		ldi		mpr, MorseLED	; If it is 0, then turn on the LEDs
		out		PORTB, mpr		;
CHECK_TOGGLE2:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 1			; Compare it to 1
		brne	CHECK_TOGGLE2	; Loop until toggle is 1
		ldi		mpr, MorseReset	; If it is 1, then turn off the LEDs
		out		PORTB, mpr		;

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: DASH
; Desc: Will check the state of toggle using polling in
;		order to display LEDs for 3 seconds and wait 1 second
;-----------------------------------------------------------
DASH:							; Begin a function with a label
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
CHECK_TOGGLE3:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 0			; Compare it to 0
		brne	CHECK_TOGGLE3	; Loop until toggle is 0
		ldi		mpr, MorseLED	; If it is 0, then turn on the LEDs
		out		PORTB, mpr		;
CHECK_TOGGLE4:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 1			; Compare it to 1
		brne	CHECK_TOGGLE4	; Loop until toggle is 1
CHECK_TOGGLE5:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 0			; Compare it to 0
		brne	CHECK_TOGGLE5	; Loop until toggle is 0
CHECK_TOGGLE6:
		mov		mpr, toggle		; Load the toggle value in
		cpi		mpr, 1			; Compare it to 1
		brne	CHECK_TOGGLE6	; Loop until toggle is 1
		ldi		mpr, MorseReset	; If it is 1, then turn off the LEDs
		out		PORTB, mpr		;

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: InitLCD2
; Desc: This function will move Strings from program memory to data memory
;		to change the second line on the LCD screen
;-----------------------------------------------------------
InitLCD2:

		ldi		ZL, $00				; Load the low byte of data memory address to ZL register
		ldi		ZH, $02				; Load the high byte of data memory address to ZH register
		ldi		YL, $10				; Load the low byte of the data memory address to YL register 
		ldi		YH, $01				; Load the high byte of the data memory address to YH register 
		ldi		XL, 16

		DO4:								; Loop through characters in memory
				ld		mpr, Z+				; Load memory from Z register to mpr and increment Z
				st		Y+, mpr				; Store character from memory to character destination and increment Y
				cpi		XL, 0				; Stop looping when reach the program memory address after last character
				dec		XL
				brne	DO4					; Loop through the do statement otherswise

		ret									; End a function with RET

;----------------------------------------------------------------
; Sub:	CONFIRM_CHAR
; Desc:	Handles functionality of the program when PD0
;		is triggered.
;----------------------------------------------------------------
CONFIRM_CHAR:
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		inc		length			; Confirm Character by incrementing word length
		ldi		mpr, 16			; Load in 16 to mpr
		cp		length, mpr		; compare the length to 16
		brne	CONTINUE		; If length is not 16, then confirm character
		rcall	BEGIN_TRANSMISSION		; at 16 characters automatically call the morse code transmission
		
CONTINUE:
		mov		YL, length			; Load the low byte of the data memory address to YL register 
		ldi		YH, $02				; Load the high byte of the data memory address to YH register 
		ldi		mpr, 0b0100_0001	; load mpr with 65 - (A)
		st		Y, mpr				; Store character from memory to character destination and increment Y

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;----------------------------------------------------------------
; Sub:	BEGIN_TRANSMISSION
; Desc:	Handles functionality of the program when PD4
;		is triggered.
;----------------------------------------------------------------
BEGIN_TRANSMISSION:
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		ldi		mpr, 0b0001_0000	; Set the transmission light on
		out		PORTB, mpr			; to the LEDs

		mov		mpr, length			; store the length in mpr
		clr		length				; empty the length
		TRANSLATE:
				rcall	TRANSLATE_CHAR	; Translate the first character at length index 0
				inc		length			; increment length
				mov		XL, length		; set XL to length
				cp		XL, mpr			; compare it to mpr (the original length)
				brlt	TRANSLATE		; loop if we have not reached the original length
				cp		XL, mpr			; compare again
				breq	TRANSLATE		; loop if we are equal to original length
DONE_TRANSMISSION:
		ldi		mpr, 0b0000_0000	; Set the transmission lights off
		out		PORTB, mpr			; to the LEDs
		rcall	SECOND_INIT_LCD		; Reset the LCD screen

		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;----------------------------------------------------------------
; Sub:	TRANSLATE_CHAR
; Desc:	Handles functionality of the program when PD4
;		is triggered.
;----------------------------------------------------------------
TRANSLATE_CHAR:
		; Save variables by pushing them to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;
				
        ldi     mpr, high(3906)	; 
        out     TCNT1H, mpr     ; high byte of TCNT1
        ldi     mpr, low(3906)  ; write the low byte
        out     TCNT1L, mpr     ;

		mov		toggle, zero	; set the toggle to zero so the lights immediately turn on

		mov		YL, length		; Load Y register to $02xx - where current character value is stored
		ldi		YH, $02			;
		ld		mpr, Y			; Load mpr with character value

		cpi		mpr, 65			; Check if char is an A
		brne	B				; 
		rcall	DOT				; dot dash
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
B:
		cpi		mpr, 66			; Check if char is an B
		brne	C				;
		rcall	DASH			; dash dot dot dot
		rcall	DOT				;
		rcall	DOT				;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
C:
		cpi		mpr, 67			; Check if char is an C
		brne	D				;
		rcall	DASH			; dash dot dash dot
		rcall	DOT				;
		rcall	DASH			;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
D:
		cpi		mpr, 68			; Check if char is an D
		brne	E				;
		rcall	DASH			; dash dot dot
		rcall	DOT				;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
E:
		cpi		mpr, 69			; Check if char is an E
		brne	F				;
		rcall	DOT				; dot
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
F:
		cpi		mpr, 70			; Check if char is an F
		brne	G				;
		rcall	DOT				; dot dot dash dot
		rcall	DOT				;
		rcall	DASH			;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
G:
		cpi		mpr, 71			; Check if char is an G
		brne	H				;
		rcall	DASH			; dash dash dot
		rcall	DASH			;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
H:
		cpi		mpr, 72			; Check if char is an H
		brne	I				;
		rcall	DOT				; dot dot dot dot
		rcall	DOT				;
		rcall	DOT				;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
I:
		cpi		mpr, 73			; Check if char is an I
		brne	J				;
		rcall	DOT				; dot dot
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
J:
		cpi		mpr, 74			; Check if char is an J
		brne	K				;
		rcall	DOT				; dot dash dash dash
		rcall	DASH			;
		rcall	DASH			;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
K:
		cpi		mpr, 75			; Check if char is an K
		brne	L				;
		rcall	DASH			; dash dot dash
		rcall	DOT				;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
L:
		cpi		mpr, 76			; Check if char is an L
		brne	M				;
		rcall	DOT				; dot dash dot dot
		rcall	DASH			;
		rcall	DOT				;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
M:
		cpi		mpr, 77			; Check if char is an M
		brne	N				;
		rcall	DASH			; dash dash
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
N:
		cpi		mpr, 78			; Check if char is an N
		brne	O				;
		rcall	DASH			; dash dot
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
O:
		cpi		mpr, 79			; Check if char is an O
		brne	P				;
		rcall	DASH			; dash dash dash
		rcall	DASH			;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
P:
		cpi		mpr, 80			; Check if char is an P
		brne	Q_				;
		rcall	DOT				; dot dash dash dot
		rcall	DASH			;
		rcall	DASH			;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
Q_:
		cpi		mpr, 81			; Check if char is an Q
		brne	R_				;
		rcall	DASH			; dash dash dot dash
		rcall	DASH			;
		rcall	DOT				;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
R_:
		cpi		mpr, 82			; Check if char is an R
		brne	S				;
		rcall	DOT				; dot dash dot
		rcall	DASH			;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
S:
		cpi		mpr, 83			; Check if char is an S
		brne	T				;
		rcall	DOT				; dot dot dot
		rcall	DOT				;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
T:
		cpi		mpr, 84			; Check if char is an T
		brne	U				;
		rcall	DASH			; dash
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
U:
		cpi		mpr, 85			; Check if char is an U
		brne	V				;
		rcall	DOT				; dot dot dash
		rcall	DOT				;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
V:
		cpi		mpr, 86			; Check if char is an V
		brne	W				;
		rcall	DOT				; dot dot dot dash
		rcall	DOT				;
		rcall	DOT				;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
W:
		cpi		mpr, 87			; Check if char is an W
		brne	X_				;
		rcall	DOT				; dot dash dash
		rcall	DASH			;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
X_:
		cpi		mpr, 88			; Check if char is an X
		brne	Y_				;
		rcall	DASH			; dash dot dot dash
		rcall	DOT				;
		rcall	DOT				;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
Y_:
		cpi		mpr, 89			; Check if char is an Y
		brne	Z_				;
		rcall	DASH			; dash dot dash dash
		rcall	DOT				;
		rcall	DASH			;
		rcall	DASH			;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
Z_:
		cpi		mpr, 90			; Check if char is an Z
		brne	DONE_TRANSLATE				;
		rcall	DASH			; dash dash dot dot
		rcall	DASH			;
		rcall	DOT				;
		rcall	DOT				;
		rcall	NEXT_CHAR		; DONE
		rjmp	DONE_TRANSLATE	;
		
DONE_TRANSLATE:
		; Restore variables by popping them from the stack,
		; in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;----------------------------------------------------------------
; Sub:	ITERATE_FORWARD
; Desc:	Handles functionality of the program when PD6
;		is triggered.
;----------------------------------------------------------------
ITERATE_FORWARD:
		push	mpr				; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		ldi		XL, $10			; Load X register to $0110 - line 2 of LCD
		ldi		XH, $01			;
		mov		YL, length		; Load Y register to $02xx - where current character value is stored
		ldi		YH, $02			;
		ld		mpr, Y			; Load mpr with character value
		inc		mpr				; Increment the character
		cpi		mpr, 0b0101_1011; Check if the current character is 91 (+Z)
		brne	RESUME_1		; If we arent past Z, continue
		ldi		mpr, 0b0100_0001; If we are moving past Z then change into 65 (A)
		
RESUME_1:
		st		Y, mpr			; Store the count back into Y ($02xx)

		rcall	InitLCD2		; Set the LCD values in memory

		rcall	LCDWrLn2		; Write out the new count to the LCD

		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr
		ret						; Return from subroutine

;----------------------------------------------------------------
; Sub:	ITERATE_BACKWARD
; Desc:	Handles functionality of the program when PD7
;		is triggered.
;----------------------------------------------------------------
ITERATE_BACKWARD:
		push	mpr				; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		ldi		XL, $10			; Load X register to $0110 - line 2 of LCD
		ldi		XH, $01			;
		mov		YL, length		; Load Y register to $02xx - where current character value is stored
		ldi		YH, $02			;
		ld		mpr, Y			; Load mpr with character value
		dec		mpr				; Increment the character
		cpi		mpr, 0b0100_0000; Check if the current character is 64 (-A)
		brne	RESUME_2		; If we aren't under A, continue
		ldi		mpr, 0b0101_1010; If we are moving under A then change into 90 (Z)
		
RESUME_2:
		st		Y, mpr			; Store the count back into Y ($02xx)

		rcall	InitLCD2		; Update the LCD in program memory

		rcall	LCDWrLn2		; Write out the new count to the LCD

		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr
		ret						; Return from subroutine

;----------------------------------------------------------------
; Sub:	WAIT_LOOP
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
WAIT_LOOP:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt			; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt			; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt			; Restore olcnt register
		pop		ilcnt			; Restore ilcnt register
		pop		waitcnt			; Restore wait register
		ret						; Return from subroutine

;-----------------------------------------------------------
; Func:	SECOND_INIT_LCD
; Desc:	This will initalize the program memory to accept inputs
;-----------------------------------------------------------
SECOND_INIT_LCD:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr				; Save mpr
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		ldi		YL, $00				; Load the low byte of the data memory address to YL register 
		ldi		YH, $02				; Load the high byte of the data memory address to YH register 
		ldi		mpr, 0b0100_0001	;
		st		Y+, mpr				; Store character from memory to character destination and increment Y
		ldi		mpr, 0b0001_0100	;
		ldi		XL, 15				; Loop through 16 times for 16 characters
		DO:							; Loop through characters in memory
				st		Y+, mpr		; Store character from memory to character destination and increment Y
				cpi		XL, 0		; Stop looping when reach the program memory address after last character
				dec		XL			;
				brne	DO			; Loop through the do statement otherswise

		clr		zero			; Initialize zero to 0
		clr		length			; Set length to 0
		ldi		waitcnt, WTime	; Wait for .15 second
		rcall	WAIT_LOOP		; Call wait function

		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

STRING1_BEG:
.DB		"Welcome!"		; Declaring data in ProgMem for welcome line1
STRING1_END:
STRING2_BEG:
.DB		"Please press PD0"	; Declaring data for welcome line1
STRING2_END:
STRING3_BEG:
.DB		"Enter word: "		; Declaring data for second LCD line1
STRING3_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

