;***********************************************************
;*
;*	Alex_Young_and_Steven_Bui_Lab4_sourcecode.asm
;*
;*	This program will check button inputs and print or clear
;*  strings to the LCD depending on which button is pressed
;*
;***********************************************************
;*
;*	 Author: Alex Young and Steven Bui
;*	   Date: 1/28/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is
								; required for LCD Driver

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

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

		; Initialize Port D for input
		ldi		mpr, $00		; Set Port D Data Direction Register
		out		DDRD, mpr		; for input
		ldi		mpr, $FF		; Initialize Port D Data Register
		out		PORTD, mpr		; so all Port D inputs are Tri-State

		; Initialize LCD Display
		rcall LCDInit			; Initialize LCD peripheral interface

		; Move strings from Program Memory to Data Memory
		rcall InitLCD1			; Call function to move strings to memory
			
		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:									; The Main program
		; Display the strings on the LCD Display
		in		mpr, PIND				; Get input from Port D
		andi	mpr, (1<<0|1<<1|1<<7)	; Check that the viable inputs are PD0, PD1, and PD7
		cpi		mpr, (1<<1|1<<7)		; Check for PD0
		brne	NEXT 					; Continue with next check (if not PD0)
		rcall   InitLCD1				; Reupdate strings in correct order for PD0
		rcall	LCDWrite				; Write strings to LCD
		rjmp	MAIN					; Continue with program

NEXT:	cpi		mpr, (1<<0|1<<7)		; Check for PD1
		brne    CLEAR					; Continue with clear check (if not PD1)
		rcall   InitLCD2				; Reupdate strings in correct order for PD1
		rcall	LCDWrite				; Write strings to LCD
		rjmp    MAIN					; Jump back to MAIN

CLEAR:
		cpi		mpr, (1<<0|1<<1)		; Check for PD7
		brne	MAIN					; No input, continue program
		rcall	LCDClr					; Clear the LCD
		rjmp	MAIN					; jump back to main and create an infinite
										; while loop.  Generally, every main program is an
										; infinite while loop, never let the main program
										; just run off

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: InitLCD1
; Desc: This function will move Strings from program memory to data memory
;		with string 1 and 2 on their respective lines of the LCD
;-----------------------------------------------------------
InitLCD1:

		ldi ZL, low(STRING1_BEG<<1)			; Load the low byte of program memory address to ZL register
		ldi ZH, high(STRING1_BEG<<1)		; Load the high byte of program memory address to ZH register
		ldi YL, $00							; Load the low byte of the data memory address to YL register 
		ldi YH, $01							; Load the high byte of the data memory address to YH register 
		
		DO1:								; Loop through characters in string 1
				lpm  mpr, Z+				; Load memory from Z register to mpr and increment Z
				st   Y+, mpr				; Store character from memory to character destination and increment Y
				cpi  ZL, low(STRING1_END<<1); Stop looping when reach the program memory address after last character
				brne DO1					; Loop through the do statement otherswise
		
		ldi ZL, low(STRING2_BEG<<1)			; Load the low byte of program memory address to ZL register
		ldi ZH, high(STRING2_BEG<<1)		; Load the high byte of program memory address to ZH register
		ldi YL, $10							; Load the low byte of the data memory address to YL register 
		ldi YH, $01							; Load the high byte of the data memory address to YH register 
		
		DO2:								; Loop through characters in string 2
				lpm  mpr, Z+				; Load memory from Z register to mpr and increment Z
				st   Y+, mpr				; Store character from memory to character destination and increment Y
				cpi  ZL, low(STRING2_END<<1); Stop looping when reach the program memory address after last character
				brne DO2					; Loop through the do statement otherswise

		ret									; End a function with RET

;-----------------------------------------------------------
; Func: InitLCD2
; Desc: This function will move Strings from program memory to data memory
;		with string 1 and 2 on their opposite lines of the LCD
;-----------------------------------------------------------
InitLCD2:

		ldi ZL, low(STRING1_BEG<<1)			; Load the low byte of program memory address to ZL register
		ldi ZH, high(STRING1_BEG<<1)		; Load the high byte of program memory address to ZH register
		ldi YL, $10							; Load the low byte of the data memory address to YL register 
		ldi YH, $01							; Load the high byte of the data memory address to YH register 
		
		DO3:								; Loop through characters in string 1
				lpm  mpr, Z+				; Load memory from Z register to mpr and increment Z
				st   Y+, mpr				; Store character from memory to character destination and increment Y
				cpi  ZL, low(STRING1_END<<1); Stop looping when reach the program memory address after last character
				brne DO3					; Loop through the do statement otherswise
		
		ldi ZL, low(STRING2_BEG<<1)			; Load the low byte of program memory address to ZL register
		ldi ZH, high(STRING2_BEG<<1)		; Load the high byte of program memory address to ZH register
		ldi YL, $00							; Load the low byte of the data memory address to YL register 
		ldi YH, $01							; Load the high byte of the data memory address to YH register
		
		DO4:								; Loop through characters in string 2
				lpm  mpr, Z+				; Load memory from Z register to mpr and increment Z
				st   Y+, mpr				; Store character from memory to character destination and increment Y
				cpi  ZL, low(STRING2_END<<1); Stop looping when reach the program memory address after last character
				brne DO4					; Loop through the do statement otherswise

		ret									; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

;-----------------------------------------------------------
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
STRING1_BEG:
.DB		"Alex_Young"		; Declaring data in ProgMem
STRING1_END:
STRING2_BEG:
.DB		"Steven_Bui"		; Declaring data in ProgMem
STRING2_END:

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

