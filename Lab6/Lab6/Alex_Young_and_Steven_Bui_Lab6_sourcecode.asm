;***********************************************************
;*
;*	Alex_Young_and_Steven_Bui_Lab6_sourcecode.asm
;*
;*	This program implements the basic bump bot functionality
;*  and movement, but with external interrupts and displaying
;*  the nuber of whisker hits on the LCD
;*
;***********************************************************
;*
;*	 Author: Alex Young and Steven Bui
;*	   Date: 2/11/21
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

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00						; Move Backward Command
.equ	TurnR = (1<<EngDirL)				; Turn Right Command
.equ	TurnL = (1<<EngDirR)				; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used
.org	$0002					; INT0
		rcall	HitRight		; PD0 triggers a HitRight on the bot
		reti					; Return from interrupt
		
.org	$0004					; INT1
		rcall	HitLeft			; PD1 triggers a HitLeft on the bot
		reti					; Return from interrupt
		
.org	$0006					; INT2
		rcall	ClearRight		; PD2 triggers a ClearRight on the bot
		reti					; Return from interrupt
		
.org	$0008					; INT3
		rcall	ClearLeft		; PD3 triggers a ClearLeft on the bot
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
			
		; Initialize LCD Display
		rcall	LCDInit			; Initialize LCD peripheral interface
		
		; Initialize Port B for output
		ldi		mpr, $FF		; Set Port B Data Direction Register
		out		DDRB, mpr		; for output
		ldi		mpr, $00		; Initialize Port B Data Register
		out		PORTB, mpr		; so all Port B outputs are low

		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Initialize TekBot Forward Movement
		ldi		mpr, MovFwd		; Load Move Forward Command
		out		PORTB, mpr		; Send command to motors

		; Initialize external interrupts
		; Set the Interrupt Sense Control to falling edge 
		ldi		mpr, 0b1010_1010
		sts		EICRA, mpr

		; Configure the External Interrupt Mask
		ldi		mpr, 0b0000_1111
		out		EIMSK, mpr

		rcall	ClearRight		; Clear/initialize the Right Buffer
		rcall	ClearLeft		; Clear/initialize the Left Buffer		

		; Turn on interrupts
		; NOTE: This must be the last thing to do in the INIT function
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		; TODO: wait for interrupts

		rjmp	MAIN			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		push	mpr				; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Move Backwards for a second
		ldi		mpr, MovBck		; Load Move Backward command
		out		PORTB, mpr		; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitLoop		; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL		; Load Turn Left Command
		out		PORTB, mpr		; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitLoop		; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd		; Load Move Forward command
		out		PORTB, mpr		; Send command to port

		ldi		XL, $00			; Load X register to $0100 - line 1 of LCD
		ldi		XH, $01			;
		ldi		YL, $00			; Load Y register to $0200 - where count of HitRight is stored
		ldi		YH, $02			;
		ld		mpr, Y			; Load mpr with count of HitRight
		inc		mpr				; Increment the count
		st		Y, mpr			; Store the count back into Y ($0200)
		rcall	Bin2ASCII		; Set $0100 with the new incremented count in ASCII
		rcall	LCDWrite		; Write out the new count to the LCD
		rcall	LCDWrite		;

		ldi		mpr, 0b0000_1111; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr		;

		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr
		ret						; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	mpr				; Save mpr register
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Move Backwards for a second
		ldi		mpr, MovBck		; Load Move Backward command
		out		PORTB, mpr		; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitLoop		; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR		; Load Turn Left Command
		out		PORTB, mpr		; Send command to port
		ldi		waitcnt, WTime	; Wait for 1 second
		rcall	WaitLoop			; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd		; Load Move Forward command
		out		PORTB, mpr		; Send command to port

		ldi		XL, $10			; Load X register to $0110 - line 1 of LCD
		ldi		XH, $01			;
		ldi		YL, $10			; Load Y register to $0210 - where count of HitLeft is stored
		ldi		YH, $02			;
		ld		mpr, Y			; Load mpr with count of HitLeft
		inc		mpr				; Increment the count
		st		Y, mpr			; Store the count back into Y ($0210)
		rcall	Bin2ASCII		; convert value in ASCII in $0100 with the new incremented count		rcall	LCDWrite		; Write out the new count to the LCD
		rcall	LCDWrite		;

		ldi		mpr, 0b0000_1111; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr		;

		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr
		ret						; Return from subroutine

;----------------------------------------------------------------
; Sub:	WaitLoop
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
WaitLoop:
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

;----------------------------------------------------------------
; Sub:	ClearRight
; Desc:	Handles functionality of the TekBot when PD2 button
;		is triggered to reset the count and LCD1
;----------------------------------------------------------------
ClearRight:

		; Save variable by pushing them to the stack
		push	mpr				; Save mpr register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		rcall	LCDClrLn1
		clr		zero
		ldi		XL, $00					; Load X register with address $0200
		ldi		XH, $02					;
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X, zero					; Set data memory to 0

		ldi		XL, $00					; Load X register with address $0100
		ldi		XH, $01					;
		ldi		YL, $00					; Load Y register with address $0200
		ldi		YH, $02					;
		ld		mpr, Y					; Load mpr with count of 0
		st		Y, mpr					; Store the 0 count in Y
		rcall	Bin2ASCII				; Convert value in ASCII and store in Line 1		rcall	LCDWrite				; Write out the Reset Right counter to LCD
		rcall	LCDWrite				;

		ldi		mpr, 0b0000_1111		; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr				;
		
		; Restore variable by popping them from the stack in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;----------------------------------------------------------------
; Sub:	ClearLeft
; Desc:	Handles functionality of the TekBot when PD3 button
;		is triggered to reset the count and LCD2
;----------------------------------------------------------------
ClearLeft:

		; Save variable by pushing them to the stack
		push	mpr				; Save mpr register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		rcall	LCDClrLn2				; Clear Line 2
		clr		zero					; Maintain zero semantics
		ldi		XL, $10					; Load X register with address $0210
		ldi		XH, $02					;
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X, zero					; Set data memory to 0

		ldi		XL, $10					; Load X register with address $0110
		ldi		XH, $01					;
		ldi		YL, $10					; Load Y register with address $0210
		ldi		YH, $02					;
		ld		mpr, Y					; Load mpr with count of 0
		st		Y, mpr					; Store the 0 count in Y
		rcall	Bin2ASCII				; Convert value in ASCII and store in Line 2		rcall	LCDWrite				; Write out the Reset Left counter to LCD
		rcall	LCDWrite				;

		ldi		mpr, 0b0000_1111		; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr				;
		
		; Restore variable by popping them from the stack in reverse order
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver
