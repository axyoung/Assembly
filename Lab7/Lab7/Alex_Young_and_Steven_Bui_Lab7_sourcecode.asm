;***********************************************************
;*
;*	Alex_Young_and_Steven_Bui_Lab7_sourcecode.asm
;*
;*	The program is intended to perform the behavior
;*  of controlling the speed of the TekBot.
;*  This is done using the PORTD buttons to control
;*  the brightness of the PORTB LEDs.
;*
;***********************************************************
;*
;*	 Author: Alex Young and Steven Bui
;*	   Date: 2/18/21
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter
.def	speed = r20				; Speed level counter

.equ	WTime = 25				; Time to wait in wait loop

.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit

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
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; place instructions in interrupt vectors here, if needed
.org	$0002					; INT0
		rcall	SPEED_DOWN		; PD0 triggers a speed down on the bot
		reti					; Return from interrupt
		
.org	$0004					; INT1
		rcall	SPEED_UP		; PD1 triggers a speed up on the bot
		reti					; Return from interrupt
		
.org	$0006					; INT2
		rcall	SPEED_MIN		; PD2 triggers a speed min on the bot
		reti					; Return from interrupt
		
.org	$0008					; INT3
		rcall	SPEED_MAX		; PD3 triggers a speed max on the bot
		reti					; Return from interrupt

.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
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
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Configure External Interrupts
		; Set the Interrupt Sense Control to falling edge 
		ldi		mpr, 0b1010_1010	; Set INT3:0 to falling edge 
		sts		EICRA, mpr			; with EICRA

		; Configure the External Interrupt Mask
		ldi		mpr, 0b0000_1111	; Turn on INTF3:0 flags
		out		EIMSK, mpr			; to enable the Port D buttosn

		; Configure 8-bit Timer/Counters
		ldi		mpr, 0b0111_1001	; Fast PWM, inverted, no prescaling timer
		out		TCCR2, mpr			'
		
		ldi		mpr, 0b0111_1001	; Fast PWM, inverted, no prescaling timer
		out		TCCR0, mpr			;

		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL)
		ldi		mpr, MovFwd			; Load Move Forward Command
		out		PORTB, mpr			; Send command to motors

		; Set initial speed, display on Port B pins 3:0
		ldi		speed, 0b0000_1111	; Initial speed level is 15
		ldi		mpr, MovFwd			; Combine the MoveForward 0110 with the speed level
		or		mpr, speed			; using OR logic
		out		PORTB, mpr			; to output to the LEDs on Port B

		ldi		mpr, 17				; Multiple 17
		mul		mpr, speed			; to the speed level
		out		OCR0, r0			; to set the motor speed level
		out		OCR2, r0			; and control the brightness of LEDs

		; Enable global interrupts (if any are used)
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		sleep
		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	SPEED_DOWN
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
SPEED_DOWN:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr				; Save mpr
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		cpi		speed, 0b0000_0000	; If the speed level is 0
		breq	RETURN				; Don't change the speed level
		dec		speed				; Otherwise, decrement the speed level

		ldi		mpr, MovFwd			; Set the Port B to a combination of MovFwd
		or		mpr, speed			; and the speed level
		out		PORTB, mpr			;

		ldi		mpr, 17				; Multiply 17 and
		mul		mpr, speed			; the speed level to get the 
		out		OCR0, r0			; Tekbot speed with the timer
		out		OCR2, r0			; To control the motor brightness
		
		ldi		waitcnt, WTime	; Wait for .1 second
		rcall	WAIT

		ldi		mpr, 0b0000_1111	; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr			;
		
		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; End a function with RET

RETURN:
		ldi		mpr, 0b0000_1111	; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr			;

		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEED_UP
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
SPEED_UP:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr				; Save mpr
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		cpi		speed, 0b0000_1111	; If the speed level is 15
		breq	RETURN2				; Don't change the speed level
		inc		speed				; Otherwise, increment the speed level

		ldi		mpr, MovFwd			; Set the Port B to a combination of MovFwd
		or		mpr, speed			; and the speed level
		out		PORTB, mpr			;

		ldi		mpr, 17				; Multiply 17 and
		mul		mpr, speed			; the speed level to get the
		out		OCR0, r0			; Tekbot speed with the timer
		out		OCR2, r0			; To control the motor brightness

		ldi		waitcnt, WTime	; Wait for .1 second
		rcall	WAIT

		ldi		mpr, 0b0000_1111	; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr			;
		
		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; End a function with RET

RETURN2:
		ldi		mpr, 0b0000_1111	; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr			;

		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEED_MIN
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
SPEED_MIN:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr				; Save mpr
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		ldi		speed, 0b0000_0000	; Set the speed level to 0
		ldi		mpr, MovFwd			; With the Bot stlil moving forward
		or		mpr, speed			;
		out		PORTB, mpr			; Output the new speed level to Port B

		ldi		mpr, 17				; Multiply 17 and
		mul		mpr, speed			; the speed level to get the
		out		OCR0, r0			; Tekbot speed with the timer
		out		OCR2, r0			; To control the motor brightness

		ldi		waitcnt, WTime	; Wait for .1 second
		rcall	WAIT

		ldi		mpr, 0b0000_1111	; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr			;

		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func:	SPEED_MAX
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
SPEED_MAX:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push	mpr				; Save mpr
		push	waitcnt			; Save wait register
		in		mpr, SREG		; Save program state
		push	mpr				;

		; Execute the function here
		ldi		speed, 0b0000_1111	; Set the speed level to 15
		ldi		mpr, MovFwd			; With the Bot stlil moving forward
		or		mpr, speed			;
		out		PORTB, mpr			; Output the new speed level to Port B

		ldi		mpr, 17				; Multiply 17 and
		mul		mpr, speed			; the speed level to get the
		out		OCR0, r0			; Tekbot speed with the timer
		out		OCR2, r0			; To control the motor brightness

		ldi		waitcnt, WTime	; Wait for .1 second
		rcall	WAIT

		ldi		mpr, 0b0000_1111	; Clear Flag register to avoid queuing interrupts
		out		EIFR, mpr			;

		; Restore any saved variables by popping from stack
		pop		mpr				; Restore program state
		out		SREG, mpr		;
		pop		waitcnt			; Restore wait register
		pop		mpr				; Restore mpr

		ret						; End a function with RET

;----------------------------------------------------------------
; Func:	WAIT
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
WAIT:
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