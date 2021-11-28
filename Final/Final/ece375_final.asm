;***********************************************************
;*	This is the final project template for ECE375 Winter 2021
;***********************************************************
;*	 Author: Your name here
;*   Date: Place date here
;***********************************************************
.include "m128def.inc"			; Include definition file
;***********************************************************
;*	Internal Register Definitions and Constants
;*	(feel free to edit these or add others)
;***********************************************************
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable
.def	mpr = r16				; Multipurpose register 
.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter
.def	index = r19				; Register used to count through loops
.def	count1 = r20			; Register for counting quotient and squares
.def	count2 = r21			; Register for counting quotient and squares
.def	count3 = r22			; Register for counting quotient and squares
.def	count4 = r23			; Register for counting quotient and squares
.def	count5 = r24			; Register for counting quotient and squares
.def	count6 = r25			; Register for counting quotient and squares

;***********************************************************
;*	Data segment variables
;*	(feel free to edit these or add others)
;***********************************************************
.dseg
.org	$0100				; data memory allocation for operands
ADD_OP1:
		.byte 6				; allocate 6 bytes for first operand of ADD
ADD_OP2:
		.byte 6				; allocate 4 bytes for second operand of ADD

.org	$0110				; data memory allocation for results
ADD_RESULT:
		.byte 7				; allocate 7 bytes for ADD result
.org	$0120				; data memory allocation for operands
SUB_OP1:
		.byte 8				; allocate 8 bytes for first operand of SUB
SUB_OP2:
		.byte 8				; allocate 8 bytes for second operand of SUB

.org	$0130				; data memory allocation for results
SUB_RESULT:
		.byte 9				; allocate 9 bytes for SUB result

.org	$0140				; data memory allocation for operands
MUL_OP1:
		.byte 6				; allocate 6 bytes for first operand of MUL
MUL_OP2:
		.byte 6				; allocate 6 bytes for second operand of MUL

.org	$0150				; data memory allocation for results
MUL_RESULT:
		.byte 12			; allocate 12 bytes for MUL result

.org	$0160				; data memory allocation for operands
DIV_OP1:
		.byte 8				; allocate 8 bytes for first operand of DIV
DIV_OP2:
		.byte 8				; allocate 8 bytes for second operand of DIV

.org	$0170				; data memory allocation for results
DIV_RESULT:
		.byte 8				; allocate 8 bytes for DIV result

.org	$0180				; data memory allocation for operands
DIV_VAR_OP1:
		.byte 8				; allocate 8 bytes for first operand of DIV
DIV_VAR_OP2:
		.byte 8				; allocate 8 bytes for second operand of DIV
		
.org	$0190				; data memory allocation for operands
SQRT_OP:
		.byte 6				; allocate 8 bytes for the operand of SQRT

.org	$01A0				; data memory allocation for results
SQRT_RESULT:
		.byte 6				; allocate 8 bytes for SQRT result

.org	$01B0				; data memory allocation for results
GM:		.byte 4				; allocate 4 bytes for GM
.org	$01C0				; data memory allocation for results
R:		.byte 2				; allocate 2 bytes for R
.org	$01D0				; data memory allocation for results
VAR:	.byte 6				; allocate 6 bytes for VAR


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment
;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt
.org	$0046					; End of Interrupt Vectors
;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:	; The initialization routine
		clr		zero
		clr		count1
		clr		count2
		clr		count3
		clr		count4
		clr		count5
		clr		count6

; To do
; your source code goes here
		; Initialize Stack Pointer
		; TODO					; Init the 2 stack pointer registers
		ldi     mpr, low(RAMEND)
        out     SPL, mpr		; Load SPL with the low byte of RAMEND
        ldi     mpr, high(RAMEND)
        out     SPH, mpr		; Load SPL with the high byte of RAMEND

		; This will store the correct GM into program memory
		rcall	STORE_GM
		rcall	CHECK_GM

		; This will store the radius into program memory
		rcall	STORE_RADIUS
		rcall	CHECK_RADIUS
		
		; This will store the gm and radius as the division operands
		ldi		ZL, low(R)					; Load low byte into ZL register
		ldi		ZH, high(R)					; Load high byte into ZH register
		ldi		XL, low(DIV_OP1)			; Load division operand address into X register, load low byte into X register
		ldi		XH, high(DIV_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(GM)					; Load low byte into ZL register
		ldi		ZH, high(GM)				; Load high byte into ZH register
		ldi		XL, low(DIV_OP2)			; Load division operand address into X register, load low byte into X register
		ldi		XH, high(DIV_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	DIV

		; Store the division result into the quotient answer space
		ldi		ZL, low(DIV_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(DIV_RESULT)		; Load high byte into ZH register
		ldi		XL, low(Quotient)			; Load quotient address into X register, load low byte into X register
		ldi		XH, high(Quotient)			; Load high byte into X register
		ldi		index, 3
		rcall	STORE_LOOP
		
		; Use the division result and square root it to get the velocity
		ldi		ZL, low(DIV_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(DIV_RESULT)		; Load high byte into ZH register
		ldi		XL, low(SQRT_OP)			; Load sqrt operand address into X register, load low byte into X register
		ldi		XH, high(SQRT_OP)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	SQRT
		rcall	CHECK_VELOCITY

		; Store the sqrt result into the velocity answer space
		ldi		ZL, low(SQRT_RESULT)		; Load low byte into ZL register
		ldi		ZH, high(SQRT_RESULT)		; Load high byte into ZH register
		ldi		XL, low(Velocity)			; Load Velocity address into X register, load low byte into X register
		ldi		XH, high(Velocity)			; Load high byte into X register
		ldi		index, 2
		rcall	STORE_LOOP

		; This will store the fourty and radius as the multiplication operands
		ldi		ZL, low(FOURTY<<1)			; Load low byte into ZL register
		ldi		ZH, high(FOURTY<<1)			; Load high byte into ZH register
		ldi		XL, low(MUL_OP1)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		ldi		ZL, low(R)					; Load low byte into ZL register
		ldi		ZH, high(R)					; Load high byte into ZH register
		ldi		XL, low(MUL_OP2)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	MULT

		; This will store the result and radius as the multiplication operands
		ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(MUL_OP1)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		ldi		ZL, low(R)					; Load low byte into ZL register
		ldi		ZH, high(R)					; Load high byte into ZH register
		ldi		XL, low(MUL_OP2)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	MULT

		; This will store the result and radius as the multiplication operands
		ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(MUL_OP1)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		ldi		ZL, low(R)					; Load low byte into ZL register
		ldi		ZH, high(R)					; Load high byte into ZH register
		ldi		XL, low(MUL_OP2)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	MULT

		; Store the mul result into the product answer space
		ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(Product)			; Load Velocity address into X register, load low byte into X register
		ldi		XH, high(Product)			; Load high byte into X register
		ldi		index, 7
		rcall	STORE_LOOP

		; This will store the result and GM as the division operands
		ldi		ZL, low(GM)					; Load low byte into ZL register
		ldi		ZH, high(GM)				; Load high byte into ZH register
		ldi		XL, low(DIV_OP1)			; Load division operand address into X register, load low byte into X register
		ldi		XH, high(DIV_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(DIV_OP2)			; Load division operand address into X register, load low byte into X register
		ldi		XH, high(DIV_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	DIV

		; This will store the result as the sqrt operand
		ldi		ZL, low(DIV_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(DIV_RESULT)		; Load high byte into ZH register
		ldi		XL, low(SQRT_OP)			; Load sqrt operand address into X register, load low byte into X register
		ldi		XH, high(SQRT_OP)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP
		
		rcall	SQRT
		rcall	CHECK_PERIOD

		; Store the sqrt result into the Period answer space
		ldi		ZL, low(SQRT_RESULT)		; Load low byte into ZL register
		ldi		ZH, high(SQRT_RESULT)		; Load high byte into ZH register
		ldi		XL, low(Period)				; Load Velocity address into X register, load low byte into X register
		ldi		XH, high(Period)			; Load high byte into X register
		ldi		index, 3
		rcall	STORE_LOOP

	
		jmp	Grading				; this should be the very last instruction of your code

;-----------------------------------------------------------
;	Procedures and Subroutines
;-----------------------------------------------------------
; your code can go here as well

;-----------------------------------------------------------
; Func: STORE_LOOP
; Desc: Loads Z values into where X register is pointing
;-----------------------------------------------------------
STORE_LOOP:
		ld		mpr, Z+
		st		X+, mpr
		dec		index
		cp		index, zero
		brne	STORE_LOOP

		ret

;-----------------------------------------------------------
; Func: STORE_LPM
; Desc: Loads Z values into where X register is pointing
;-----------------------------------------------------------
STORE_LPM:
		lpm		mpr, Z+
		st		X+, mpr
		dec		index
		cp		index, zero
		brne	STORE_LPM

		ret

;-----------------------------------------------------------
; Func: CLR_LOOP
; Desc: Clears program memory for each index space
;-----------------------------------------------------------
CLR_LOOP:
		st		X+, zero		; Set data memory to 0 and inc X
		dec		index
		cp		index, zero
		brne	CLR_LOOP

		ret

;-----------------------------------------------------------
; Func: STORE_COUNTERS
; Desc: Stores the count values into memory
;-----------------------------------------------------------
STORE_COUNTERS:
		st		X+, ZL
		st		X+, ZH
		mov		ZL, count3					; Load low byte into ZL register
		mov		ZH, count4					; Load high byte into ZH register
		st		X+, ZL
		st		X+, ZH
		mov		ZL, count5					; Load low byte into ZL register
		mov		ZH, count6					; Load high byte into ZH register
		st		X+, ZL
		st		X+, ZH
		mov		ZL, zero					; Load low byte into ZL register
		mov		ZH, zero					; Load high byte into ZH register
		st		X+, ZL
		st		X+, ZH

		ret

;-----------------------------------------------------------
; Func: STORE_GM
; Desc: Stores GM value into program memory
;-----------------------------------------------------------
STORE_GM:
		ldi		ZH, high(SelectedPlanet<<1)
		ldi		ZL, low(SelectedPlanet<<1)
		lpm		mpr, Z
		ldi		XL, 4
		mul		mpr, XL				; this will set rlo to the value to shift over to find the selected GM
		ldi		ZH, high(PlanetInfo<<1)
		ldi		mpr, low(PlanetInfo<<1)
		add		mpr, rlo
		brcc	CONTINUE
		inc		ZH

CONTINUE:
		mov		ZL, mpr
		ldi		XL, low(GM)
		ldi		XH, high(GM)
		ldi		index, 4
		rcall	STORE_LPM

		ret

;-----------------------------------------------------------
; Func: CHECK_GM
; Desc: Checks if GM is > 1000 by seeing if GM-1000 is positive or 0
;-----------------------------------------------------------
CHECK_GM:
		ldi		ZL, low(THOUSANDX<<1)		; Load low byte into ZL register
		ldi		ZH, high(THOUSANDX<<1)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LPM

		ldi		ZL, low(GM)					; Load low byte into ZL register
		ldi		ZH, high(GM)				; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT
		
		ldi		ZH, high(SUB_RESULT)
		ldi		ZL, low(SUB_RESULT)
		ldi		index, 4
		GM_CHECK_ZERO:
				ld		mpr, Z+
				cp		mpr, zero
				brne	GM_CHECK_NEG
				dec		index
				cp		index, zero
				brne	GM_CHECK_ZERO
GM_BAD:
		; Store -1 into the period answer space
		ldi		ZL, low(NEG_ONE<<1)			; Load low byte into ZL register
		ldi		ZH, high(NEG_ONE<<1)		; Load high byte into ZH register
		ldi		XL, low(Period)				; Load Period address into X register, load low byte into X register
		ldi		XH, high(Period)			; Load high byte into X register
		ldi		index, 3
		rcall	STORE_LPM

		jmp		Grading

GM_CHECK_NEG:
		rcall	LOAD_CARRY
		brne	GM_BAD

GM_GOOD:
		ret

;-----------------------------------------------------------
; Func: STORE_RADIUS
; Desc: Stores orbital radius into program memory
;-----------------------------------------------------------
STORE_RADIUS:
		ldi		ZL, low(OrbitalRadius<<1)
		ldi		XL, low(SelectedPlanet<<1)
		ldi		mpr, 2
		add		ZL, mpr
		cp		XL, ZL
		breq	NORMAL_RADIUS

		ldi		ZL, low(OrbitalRadius<<1)
		ldi		ZH, high(OrbitalRadius<<1)
		ldi		XL, low(R)
		ldi		XH, high(R)
		ldi		index, 3
		rcall	STORE_LPM

NORMAL_RADIUS:
		ldi		ZL, low(OrbitalRadius<<1)
		ldi		ZH, high(OrbitalRadius<<1)
		ldi		XL, low(R)
		ldi		XH, high(R)
		ldi		index, 2
		rcall	STORE_LPM

		ret

;-----------------------------------------------------------
; Func: CHECK_RADIUS
; Desc: Checks if R is > 1000 by seeing if R-1000 is positive or 0
;-----------------------------------------------------------
CHECK_RADIUS:
		ldi		ZL, low(THOUSANDX<<1)		; Load low byte into ZL register
		ldi		ZH, high(THOUSANDX<<1)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LPM

		ldi		ZL, low(R)					; Load low byte into ZL register
		ldi		ZH, high(R)					; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT
		
		ldi		ZH, high(SUB_RESULT)
		ldi		ZL, low(SUB_RESULT)
		ldi		index, 4
		RADIUS_CHECK_ZERO:
				ld		mpr, Z+
				cp		mpr, zero
				brne	RADIUS_CHECK_NEG
				dec		index
				cp		index, zero
				brne	RADIUS_CHECK_ZERO
RADIUS_BAD:
		; Store -1 into the period answer space
		ldi		ZL, low(NEG_ONE<<1)			; Load low byte into ZL register
		ldi		ZH, high(NEG_ONE<<1)		; Load high byte into ZH register
		ldi		XL, low(Velocity)			; Load Velocity address into X register, load low byte into X register
		ldi		XH, high(Velocity)			; Load high byte into X register
		ldi		index, 2
		rcall	STORE_LPM

		jmp		Grading

RADIUS_CHECK_NEG:
		rcall	LOAD_CARRY
		brne	RADIUS_BAD

RADIUS_GOOD:
		ret

;-----------------------------------------------------------
; Func: CHECK_VELOCITY
; Desc: If the velocity is 0 then return negative -2
;-----------------------------------------------------------
CHECK_VELOCITY:
		ldi		ZH, high(SQRT_RESULT)
		ldi		ZL, low(SQRT_RESULT)
		ldi		index, 6
		VELOCITY_CHECK_ZERO:
				ld		mpr, Z+
				cp		mpr, zero
				brne	VELOCITY_GOOD
				dec		index
				cp		index, zero
				brne	VELOCITY_CHECK_ZERO
		; Store -2 into the period answer space
		ldi		ZL, low(NEG_TWO<<1)			; Load low byte into ZL register
		ldi		ZH, high(NEG_TWO<<1)		; Load high byte into ZH register
		ldi		XL, low(Velocity)			; Load Velocity address into X register, load low byte into X register
		ldi		XH, high(Velocity)			; Load high byte into X register
		ldi		index, 2
		rcall	STORE_LPM

		jmp		Grading

VELOCITY_GOOD:	ret

;-----------------------------------------------------------
; Func: CHECK_PERIOD
; Desc: If the period is less than 25 then return negative -2
;-----------------------------------------------------------
CHECK_PERIOD:
		ldi		ZL, low(TWENTYFIVE<<1)		; Load low byte into ZL register
		ldi		ZH, high(TWENTYFIVE<<1)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LPM

		ldi		ZL, low(SQRT_RESULT)		; Load low byte into ZL register
		ldi		ZH, high(SQRT_RESULT)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT
		
		rcall	LOAD_CARRY
		breq	PERIOD_GOOD

		; Store -2 into the period answer space
		ldi		ZL, low(NEG_TWO<<1)			; Load low byte into ZL register
		ldi		ZH, high(NEG_TWO<<1)		; Load high byte into ZH register
		ldi		XL, low(Period)				; Load Period address into X register, load low byte into X register
		ldi		XH, high(Period)			; Load high byte into X register
		ldi		index, 3
		rcall	STORE_LPM

		jmp		Grading

PERIOD_GOOD:	ret

;-----------------------------------------------------------
; Func: LOAD_CARRY
; Desc: Loads in the carry value to compare to zero
;-----------------------------------------------------------
LOAD_CARRY:
		ldi		mpr, low(SUB_RESULT)
		ldi		XL, 8
		add		mpr, XL
		mov		ZL, mpr
		ldi		ZH, high(SUB_RESULT)
		ld		mpr, Z
		cp		mpr, zero

		ret

;-----------------------------------------------------------
; Func: SUB_VAR
; Desc: Subtracts var from count
;-----------------------------------------------------------
SUB_VAR:
		ldi		XL, low(SUB_OP1)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		mov		ZL, count1					; Load low byte into ZL register
		mov		ZH, count2					; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		rcall	STORE_COUNTERS

		rcall	SUBT
		
		ldi		ZL, low(SUB_RESULT)				; Load low byte into ZL register
		ldi		ZH, high(SUB_RESULT)			; Load high byte into ZH register
		ld		mpr, Z+
		mov		count1, mpr
		ld		mpr, Z+
		mov		count2, mpr
		ld		mpr, Z+
		mov		count3, mpr
		ld		mpr, Z+
		mov		count4, mpr
		ld		mpr, Z+
		mov		count5, mpr
		ld		mpr, Z+
		mov		count6, mpr
		
		ldi		ZL, low(VAR)
		ldi		ZH, high(VAR)
		ldi		XL, low(SUB_OP1)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(DIV_RESULT)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT

		ldi		ZL, low(SUB_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(SUB_RESULT)		; Load high byte into ZH register
		ldi		XL, low(DIV_RESULT)			; Load div result operand address into X register, load low byte into X register
		ldi		XH, high(DIV_RESULT)		; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ret
;-----------------------------------------------------------
; Func: ADD_VAR
; Desc: Adds variable amount to count
;-----------------------------------------------------------
ADD_VAR:
		ldi		XL, low(ADD_OP1)			; Load add operand address into X register, load low byte into X register
		ldi		XH, high(ADD_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		mov		ZL, count1					; Load low byte into ZL register
		mov		ZH, count2					; Load high byte into ZH register
		ldi		XL, low(ADD_OP2)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(ADD_OP2)			; Load high byte into X register
		rcall	STORE_COUNTERS

		rcall	ADD_

		ldi		ZL, low(ADD_RESULT)				; Load low byte into ZL register
		ldi		ZH, high(ADD_RESULT)			; Load high byte into ZH register
		ld		mpr, Z+
		mov		count1, mpr
		ld		mpr, Z+
		mov		count2, mpr
		ld		mpr, Z+
		mov		count3, mpr
		ld		mpr, Z+
		mov		count4, mpr
		ld		mpr, Z+
		mov		count5, mpr
		ld		mpr, Z+
		mov		count6, mpr

		; Update temp_count
		ldi		ZL, low(VAR)
		ldi		ZH, high(VAR)
		ldi		XL, low(ADD_OP1)			; Load add operand address into X register, load low byte into X register
		ldi		XH, high(ADD_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(DIV_RESULT)		; Load high byte into ZH register
		ldi		XL, low(ADD_OP2)			; Load add operand address into X register, load low byte into X register
		ldi		XH, high(ADD_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	ADD_

		ldi		ZL, low(ADD_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(ADD_RESULT)		; Load high byte into ZH register
		ldi		XL, low(DIV_RESULT)			; Load div result operand address into X register, load low byte into X register
		ldi		XH, high(DIV_RESULT)		; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ret
		
;-----------------------------------------------------------
; Func: SUBCOUNT
; Desc: Subtracts the count from DIV_OP 2
;-----------------------------------------------------------
SUB_COUNT:
		cp		count1, zero
		brbc	1, COUNT_NOT_ZERO
		cp		count2, zero
		brbc	1, COUNT_NOT_ZERO
		cp		count3, zero
		brbc	1, COUNT_NOT_ZERO
		cp		count4, zero
		brbc	1, COUNT_NOT_ZERO
		cp		count5, zero
		brbc	1, COUNT_NOT_ZERO
		cp		count6, zero
		brbc	1, COUNT_NOT_ZERO
		ret
COUNT_NOT_ZERO:
		ldi		ZL, low(DIV_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(DIV_RESULT)		; Load high byte into ZH register
		ldi		XL, low(MUL_OP1)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_OP1)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP1)			; Load high byte into ZH register
		ldi		XL, low(MUL_OP2)			; Load mult operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	MULT

		ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_OP2)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP2)			; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT
		
		ldi		ZL, low(SUB_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(SUB_RESULT)		; Load high byte into ZH register
		ldi		XL, low(DIV_OP2)			; Load div op2 operand address into X register, load low byte into X register
		ldi		XH, high(DIV_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ret

;-----------------------------------------------------------
; Func: DIV
; Desc: Divides two 64-bit numbers and generates a 48-bit
;		result.
;-----------------------------------------------------------
DIV:
		push	zero					; Save zero register
		push	XH						; Save XH-ptr
		push	XL						; Save XL-ptr
		push	YH						; Save YH-ptr
		push	YL						; Save YL-ptr
		push	ZH						; Save ZH-ptr
		push	ZL						; Save ZL-ptr
		push	mpr						; Save mpr

		clr		zero					; Maintain zero semantics
		ldi		XL, low(DIV_RESULT)		; Clear low byte of result data memory
		ldi		XH, high(DIV_RESULT)	; Clear high byte of result data memory
		ldi		index, 8
		rcall	CLR_LOOP

		; now we need to load a billion into VAR - but only if radius is less than 1 million
		ldi		ZL, low(BIL<<1)				; Load low byte into ZL register
		ldi		ZH, high(BIL<<1)			; Load high byte into ZH register
		ldi		XL, low(VAR)				; Load low byte into X register
		ldi		XH, high(VAR)				; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		rcall	DIV_VAR
		
		rcall	SUB_COUNT

		; now we need to load a million into VAR 
		ldi		ZL, low(MIL<<1)				; Load low byte into ZL register
		ldi		ZH, high(MIL<<1)			; Load high byte into ZH register
		ldi		XL, low(VAR)				; Load low byte into X register
		ldi		XH, high(VAR)				; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		rcall	DIV_VAR

		rcall	SUB_COUNT

		; now we need to load 1000 into VAR 
		ldi		ZL, low(THOUSAND<<1)		; Load low byte into ZL register
		ldi		ZH, high(THOUSAND<<1)		; Load high byte into ZH register
		ldi		XL, low(VAR)				; Load var operand address into X register, load low byte into X register
		ldi		XH, high(VAR)				; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		rcall	DIV_VAR
		
		rcall	SUB_COUNT
		
		; now we need to load 1 into VAR 
		ldi		ZL, low(ONE<<1)				; Load low byte into ZL register
		ldi		ZH, high(ONE<<1)			; Load high byte into ZH register
		ldi		XL, low(VAR)				; Load var operand address into X register, load low byte into X register
		ldi		XH, high(VAR)				; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		; This will store the divide operands to subtract operands
		ldi		ZL, low(DIV_OP1)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP1)			; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_OP2)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP2)			; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT
		
		; Here I exit if the carry value is 1 (meaning a negative result)
		rcall	LOAD_CARRY
		breq	ADD_INIT
		rjmp	EXIT

		ADD_INIT:
				ldi		ZL, low(VAR)				; Load low byte into ZL register
				ldi		ZH, high(VAR)				; Load high byte into ZH register
				rcall	ADD_VAR

		DIV_LOOP:
				ldi		ZL, low(VAR)				; Load low byte into ZL register
				ldi		ZH, high(VAR)				; Load high byte into ZH register
				rcall	ADD_VAR

		CONT:
				ldi		ZL, low(DIV_OP1)			; Load low byte into ZL register
				ldi		ZH, high(DIV_OP1)			; Load high byte into ZH register
				ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
				ldi		XH, high(SUB_OP1)			; Load high byte into X register
				ldi		index, 8
				rcall	STORE_LOOP

				ldi		ZL, low(SUB_RESULT)			; Load low byte into ZL register
				ldi		ZH, high(SUB_RESULT)		; Load high byte into ZH register
				ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
				ldi		XH, high(SUB_OP2)			; Load high byte into X register
				ldi		index, 8
				rcall	STORE_LOOP

				rcall	SUBT
													; Here we compare the carry bit to check if the value is negative
				ldi		ZH, high(SUB_RESULT)
				ldi		ZL, low(SUB_RESULT)
				ldi		index, 8
				CHECK_ZERO:
						ld		mpr, Z+
						cp		mpr, zero
						brne	CHECK_CARRY
						dec		index
						cp		index, zero
						brne	CHECK_ZERO
						ld		mpr, Z+
						cp		mpr, zero
						rjmp	EXIT
		CHECK_CARRY:
				rcall	LOAD_CARRY
				breq	DIV_LOOP

		; now we need to double the remainder in order to compare it to op1, if less than, decrement the count
		ldi		ZL, low(TWO<<1)				; Load low byte into ZL register
		ldi		ZH, high(TWO<<1)			; Load high byte into ZH register
		ldi		XL, low(MUL_OP1)			; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		ldi		ZL, low(SUB_OP2)			; Load low byte into ZL register
		ldi		ZH, high(SUB_OP2)			; Load high byte into ZH register
		ldi		XL, low(MUL_OP2)			; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	MULT

		; compare it to op2 by subtracting op 1
		ldi		ZL, low(DIV_OP1)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP1)			; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(MUL_RESULT)		; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load sub operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT

		; Here I check to the carry value - if it is 1, that means I need to subtract 1 from the quotient
		rcall	LOAD_CARRY
		breq	EXIT

		; Here I subtract 1 from the quotient value		
		ldi		ZL, low(VAR)				; Load low byte into ZL register
		ldi		ZH, high(VAR)				; Load high byte into ZH register
		rcall	SUB_VAR

		; Here I store the result from subtraction into my division results
		ldi		ZL, low(SUB_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(SUB_RESULT)		; Load high byte into ZH register
		ldi		XL, low(DIV_RESULT)			; Load div result address into X register, load low byte into X register
		ldi		XH, high(DIV_RESULT)		; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		EXIT:
				mov		ZL, count1					; Load low byte into ZL register
				mov		ZH, count2					; Load high byte into ZH register
				ldi		XL, low(DIV_RESULT)			; Load div result address into X register, load low byte into X register
				ldi		XH, high(DIV_RESULT)		; Load high byte into X register
				rcall	STORE_COUNTERS

				clr		count1
				clr		count2
				clr		count3
				clr		count4
				clr		count5
				clr		count6
				
				pop		mpr						; Restore all registers in reverves order
				pop		ZL				
				pop		ZH
				pop		YL
				pop		YH
				pop		XL
				pop		XH
				pop		zero
				ret
;-----------------------------------------------------------
; Func: DIV_VAR
; Desc: Divides two 64-bit numbers by decrementing by a variable amount
;-----------------------------------------------------------
DIV_VAR:
		push	zero					; Save zero register
		push	XH						; Save XH-ptr
		push	XL						; Save XL-ptr
		push	YH						; Save YH-ptr
		push	YL						; Save YL-ptr
		push	ZH						; Save ZH-ptr
		push	ZL						; Save ZL-ptr
		push	mpr						; Save mpr
		
		clr		zero					; Maintain zero semantics
		ldi		XL, low(DIV_RESULT)		; Clear low byte of result data memory
		ldi		XH, high(DIV_RESULT)	; Clear high byte of result data memory
		ldi		index, 8
		rcall	CLR_LOOP
				
		; now we need to multiply the op1 by VAR 
		ldi		ZL, low(VAR)				; Load low byte into ZL register
		ldi		ZH, high(VAR)				; Load high byte into ZH register
		ldi		XL, low(MUL_OP1)			; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP1)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_OP1)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP1)			; Load high byte into ZH register
		ldi		XL, low(MUL_OP2)			; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(MUL_OP2)			; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LOOP

		rcall	MULT
		
		; Here I store the result from multiply into my division operand and copy the op2 from div
		ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
		ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
		ldi		XL, low(DIV_VAR_OP1)		; Load div operand address into X register, load low byte into X register
		ldi		XH, high(DIV_VAR_OP1)		; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP
		
		ldi		ZL, low(DIV_OP2)			; Load low byte into ZL register
		ldi		ZH, high(DIV_OP2)			; Load high byte into ZH register
		ldi		XL, low(DIV_VAR_OP2)		; Load div operand address into X register, load low byte into X register
		ldi		XH, high(DIV_VAR_OP2)		; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP
		
		; This will store the divide operands to subtract operands
		ldi		ZL, low(DIV_VAR_OP1)		; Load low byte into ZL register
		ldi		ZH, high(DIV_VAR_OP1)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP1)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		ldi		ZL, low(DIV_VAR_OP2)		; Load low byte into ZL register
		ldi		ZH, high(DIV_VAR_OP2)		; Load high byte into ZH register
		ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB_OP2)			; Load high byte into X register
		ldi		index, 8
		rcall	STORE_LOOP

		rcall	SUBT

		; Here I exit if the carry value is 1 (meaning a negative result)
		rcall	LOAD_CARRY
		breq	ADD_INIT_VAR
		rjmp	EXIT_VAR
		
		ADD_INIT_VAR:
				ldi		ZL, low(VAR)				; Load low byte into ZL register
				ldi		ZH, high(VAR)				; Load high byte into ZH register
				rcall	ADD_VAR

		DIV_LOOP_VAR:
				ldi		ZL, low(VAR)				; Load low byte into ZL register
				ldi		ZH, high(VAR)				; Load high byte into ZH register
				rcall	ADD_VAR

		CONT_VAR:
				ldi		ZL, low(DIV_VAR_OP1)		; Load low byte into ZL register
				ldi		ZH, high(DIV_VAR_OP1)		; Load high byte into ZH register
				ldi		XL, low(SUB_OP1)			; Load subtraction operand address into X register, load low byte into X register
				ldi		XH, high(SUB_OP1)			; Load high byte into X register
				ldi		index, 8
				rcall	STORE_LOOP

				ldi		ZL, low(SUB_RESULT)			; Load low byte into ZL register
				ldi		ZH, high(SUB_RESULT)		; Load high byte into ZH register
				ldi		XL, low(SUB_OP2)			; Load subtraction operand address into X register, load low byte into X register
				ldi		XH, high(SUB_OP2)			; Load high byte into X register
				ldi		index, 8
				rcall	STORE_LOOP

				rcall	SUBT
													; Here we compare the carry bit to check if the value is negative
				ldi		ZH, high(SUB_RESULT)
				ldi		ZL, low(SUB_RESULT)
				ldi		index, 8
				CHECK_ZERO_VAR:
						ld		mpr, Z+
						cp		mpr, zero
						brne	CHECK_CARRY_VAR
						dec		index
						cp		index, zero
						brne	CHECK_ZERO_VAR
						ld		mpr, Z+
						cp		mpr, zero
						rjmp	EXIT_VAR
		CHECK_CARRY_VAR:
				rcall	LOAD_CARRY
				breq	DIV_LOOP_VAR
				
		; Here I subtract VAR from the quotient value
		ldi		ZL, low(VAR)				; Load low byte into ZL register
		ldi		ZH, high(VAR)				; Load high byte into ZH register
		rcall	SUB_VAR

		EXIT_VAR:				
				pop		mpr						; Restore all registers in reverves order
				pop		ZL				
				pop		ZH
				pop		YL
				pop		YH
				pop		XL
				pop		XH
				pop		zero
				ret

;-----------------------------------------------------------
; Func: SQRT
; Desc: Square roots a 48-bit number and generates a 48-bit
;		result.
;-----------------------------------------------------------
SQRT:
		push	zero					; Save zero register
		push	XH						; Save XH-ptr
		push	XL						; Save XL-ptr
		push	YH						; Save YH-ptr
		push	YL						; Save YL-ptr
		push	ZH						; Save ZH-ptr
		push	ZL						; Save ZL-ptr
		push	mpr						; Save mpr

		clr		zero					; Maintain zero semantics
		ldi		XL, low(SQRT_RESULT)	; Clear low byte of result data memory
		ldi		XH, high(SQRT_RESULT)	; Clear high byte of result data memory
		ldi		index, 6
		rcall	CLR_LOOP
		
		; now we need to load 100 into VAR 
		ldi		ZL, low(HUNDRED<<1)			; Load low byte into ZL register
		ldi		ZH, high(HUNDRED<<1)		; Load high byte into ZH register
		ldi		XL, low(VAR)				; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(VAR)				; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		rcall	SQRT_VAR

		; now we need to load 1 into VAR 
		ldi		ZL, low(ONE<<1)				; Load low byte into ZL register
		ldi		ZH, high(ONE<<1)			; Load high byte into ZH register
		ldi		XL, low(VAR)				; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(VAR)				; Load high byte into X register
		ldi		index, 6
		rcall	STORE_LPM

		rcall	SQRT_VAR

		mov		ZL, count1					; Load low byte into ZL register
		mov		ZH, count2					; Load high byte into ZH register
		ldi		XL, low(SQRT_RESULT)		; Load sqrt operand address into X register, load low byte into X register
		ldi		XH, high(SQRT_RESULT)		; Load high byte into X register
		rcall	STORE_COUNTERS
		
		clr		count1
		clr		count2
		clr		count3
		clr		count4
		clr		count5
		clr		count6
						
		pop		mpr						; Restore all registers in reverves order
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		ret

;-----------------------------------------------------------
; Func: SQRT_VAR
; Desc: Square roots a 48-bit number and generates a 48-bit
;		result.
;-----------------------------------------------------------
SQRT_VAR:	
		SQRT_LOOP:
				mov		ZL, count1					; Load low byte into ZL register
				mov		ZH, count2					; Load high byte into ZH register
				ldi		XL, low(MUL_OP1)			; Load sqrt operand address into X register, load low byte into X register
				ldi		XH, high(MUL_OP1)			; Load high byte into X register
				rcall	STORE_COUNTERS
				
				mov		ZL, count1					; Load low byte into ZL register
				mov		ZH, count2					; Load high byte into ZH register
				ldi		XL, low(MUL_OP2)			; Load sqrt operand address into X register, load low byte into X register
				ldi		XH, high(MUL_OP2)			; Load high byte into X register
				rcall	STORE_COUNTERS
				
				rcall	MULT
				
				; compare it to sqrt OP by subtracting
				ldi		ZL, low(MUL_RESULT)			; Load low byte into ZL register
				ldi		ZH, high(MUL_RESULT)		; Load high byte into ZH register
				ldi		XL, low(SUB_OP1)			; Load multiplication operand address into X register, load low byte into X register
				ldi		XH, high(SUB_OP1)			; Load high byte into X register
				ldi		index, 8
				rcall	STORE_LOOP

				ldi		ZL, low(SQRT_OP)			; Load low byte into ZL register
				ldi		ZH, high(SQRT_OP)			; Load high byte into ZH register
				ldi		XL, low(SUB_OP2)			; Load multiplication operand address into X register, load low byte into X register
				ldi		XH, high(SUB_OP2)			; Load high byte into X register
				ldi		index, 8
				rcall	STORE_LOOP

				rcall	SUBT

				; Here I check to the carry value - if it is 1, that means I need to subtract 1 from the count
				rcall	LOAD_CARRY
				brne	FINISH_SQRT
				
				ldi		ZL, low(VAR)				; Load low byte into ZL register
				ldi		ZH, high(VAR)				; Load high byte into ZH register
				rcall	ADD_VAR
				rjmp	SQRT_LOOP


		FINISH_SQRT:
				; Here I subtract VAR from the quotient value
				ldi		ZL, low(VAR)				; Load low byte into ZL register
				ldi		ZH, high(VAR)				; Load high byte into ZH register
				rcall	SUB_VAR

				; Here I store the result from subtraction into my sqrt results
				ldi		ZL, low(SUB_RESULT)			; Load low byte into ZL register
				ldi		ZH, high(SUB_RESULT)		; Load high byte into ZH register
				ldi		XL, low(SQRT_RESULT)		; Load sqrt operand address into X register, load low byte into X register
				ldi		XH, high(SQRT_RESULT)		; Load high byte into X register
				ldi		index, 6
				rcall	STORE_LOOP
		
		ret

;-----------------------------------------------------------
; Func: SUBT
; Desc: Subtracts two 64-bit numbers and generates a 64-bit
;		result with a carry sign.
;-----------------------------------------------------------
SUBT:
		; Execute the function here
		push	zero					; Save zero register
		push	XH						; Save XH-ptr
		push	XL						; Save XL-ptr
		push	YH						; Save YH-ptr
		push	YL						; Save YL-ptr
		push	ZH						; Save ZH-ptr
		push	ZL						; Save ZL-ptr
		push	mpr						; Save mpr
		push	r20
		push	r21

		clr		zero					; Maintain zero semantics
		ldi		XL, low(SUB_RESULT)		; Clear low byte of result data memory
		ldi		XH, high(SUB_RESULT)	; Clear high byte of result data memory
		ldi		index, 9
		rcall	CLR_LOOP

		; Load beginning address of first operand into X
		ldi		XL, low(SUB_OP1)		; Load low byte of address
		ldi		XH, high(SUB_OP1)		; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(SUB_OP2)		; Load low byte of address
		ldi		YH, high(SUB_OP2)		; Load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(SUB_RESULT)		; Load low byte of address
		ldi		ZH, high(SUB_RESULT)	; Load high byte of address

		; Execute the function here
        ld      r20, X+					; Load register 20 with 1rst byte of op1
        ld      r21, Y+					; Load register 21 with 1rst byte of op2
        sub     r21, r20				; Subtract righthandside of equation
        st      Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 2nd byte of op1
        ld      r21, Y+					; Load register 21 with 2nd byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 3rd byte of op1
        ld      r21, Y+					; Load register 21 with 3rd byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 4th byte of op1
        ld      r21, Y+					; Load register 21 with 4th byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
		st      Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 5th byte of op1
        ld      r21, Y+					; Load register 21 with 5th byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 6th byte of op1
        ld      r21, Y+					; Load register 21 with 6th byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
		st      Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 7th byte of op1
        ld      r21, Y+					; Load register 21 with 7th byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X					; Load register 20 with 8th byte of op1
        ld      r21, Y					; Load register 21 with 8th byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
		st		Z+, r21					; Store result
        brcc	EXIT2					; If no carry, return to caller
        st		Z, XH					; Store carry result if it exists

EXIT2:
		pop		r21
		pop		r20
		pop		mpr						; Restore all registers in reverves order
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
        ret								; End a function with RET

;-----------------------------------------------------------
; Func: ADD_
; Desc: Adds two 48-bit numbers and generates a 54-bit number
;		where the high byte of the result contains the carry
;		out bit.
;-----------------------------------------------------------
ADD_:
		push	zero					; Save zero register
		push	XH						; Save XH-ptr
		push	XL						; Save XL-ptr
		push	YH						; Save YH-ptr
		push	YL						; Save YL-ptr
		push	ZH						; Save ZH-ptr
		push	ZL						; Save ZL-ptr
		push	mpr						; Save mpr
		push	r20
		push	r21

		clr		zero					; Maintain zero semantics
		ldi		XL, low(ADD_RESULT)		; Clear low byte of result data memory
		ldi		XH, high(ADD_RESULT)	; Clear high byte of result data memory
		ldi		index, 7
		rcall	CLR_LOOP

		; Load beginning address of first operand into X
		ldi		XL, low(ADD_OP1)		; Load low byte of address
		ldi		XH, high(ADD_OP1)		; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(ADD_OP2)		; Load low byte of address
		ldi		YH, high(ADD_OP2)		; Load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(ADD_RESULT)	; Load low byte of address
		ldi		ZH, high(ADD_RESULT)	; Load high byte of address

		; Execute the function here
        ld      r20, X+					; Load register 20 with 1rst byte of op1
        ld      r21, Y+					; Load register 21 with 1rst byte of op2
        add     r21, r20				; Add together righthandside of equation
        st      Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 2nd byte of op1
        ld      r21, Y+					; Load register 21 with 2nd byte of op2
        adc		r21, r20				; Add together lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 3rd byte of op1
        ld      r21, Y+					; Load register 21 with 3rd byte of op2
        adc		r21, r20				; Add together lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 4th byte of op1
        ld      r21, Y+					; Load register 21 with 4th byte of op2
        adc		r21, r20				; Add together lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X+					; Load register 20 with 5th byte of op1
        ld      r21, Y+					; Load register 21 with 5th byte of op2
        adc		r21, r20				; Add together lefthandside of equation with carry
        st		Z+, r21					; Store result
        ld      r20, X					; Load register 20 with 6th byte of op1
        ld      r21, Y					; Load register 21 with 6th byte of op2
        adc		r21, r20				; Add together lefthandside of equation with carry
        st		Z+, r21					; Store result
        brcc	EXIT3					; If no carry, return to caller
        st		Z, XH					; Store carry result if it exists

EXIT3:
		pop		r21
		pop		r20
		pop		mpr						; Restore all registers in reverves order
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
        ret								; End a function with RET


;-----------------------------------------------------------
; Func: MULT
; Desc: Multiplies two 48-bit numbers and generates a 96-bit 
;		result.
;-----------------------------------------------------------
MULT:
		; Execute the function here
		push 	A						; Save A register
		push	B						; Save B register
		push	rhi						; Save rhi register
		push	rlo						; Save rlo register
		push	zero					; Save zero register
		push	XH						; Save XH-ptr
		push	XL						; Save XL-ptr
		push	YH						; Save YH-ptr
		push	YL						; Save YL-ptr
		push	ZH						; Save ZH-ptr
		push	ZL						; Save ZL-ptr
		push	oloop					; Save outer counter
		push	iloop					; Save inner counter
		push	mpr						; Save mpr
		push	r20						; Save r20
		push	r21						; Save r21

		clr		zero					; Maintain zero semantics
		ldi		XL, low(MUL_RESULT)		; Clear low byte of result data memory
		ldi		XH, high(MUL_RESULT)	; Clear high byte of result data memory
		ldi		index, 12
		rcall	CLR_LOOP

		; Set Y to beginning address of B
		ldi		YL, low(MUL_OP2)		; Load low byte
		ldi		YH, high(MUL_OP2)		; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL_RESULT)		; Load low byte
		ldi		ZH, high(MUL_RESULT)	; Load high byte

		; Begin outer for loop
		ldi		oloop, 6				; Load counter for 8 loops
MUL_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(MUL_OP1)		; Load low byte
		ldi		XH, high(MUL_OP1)		; Load high byte

		; Begin inner for loop
		ldi		iloop, 6				; Load counter for 8 loops
MUL_ILOOP:
		ld		A, X+					; Get byte of A operand
		ld		B, Y					; Get byte of B operand
		mul		A,B						; Multiply A and B
		ld		A, Z+					; Get a result byte from memory
		ld		B, Z+					; Get the next result byte from memory
		add		rlo, A					; rlo <= rlo + A
		adc		rhi, B					; rhi <= rhi + B + carry
		ld		A, Z+					; Get a third byte from the result
		adc		A, zero					; Add carry to A
		ld		B, Z+					; Get a fourth byte from the result
		adc		B, zero					; Add carry to B
		ld		mpr, Z+					; Get a fifth byte from the result
		adc		mpr, zero				; Add carry to mpr
		ld		r20, Z+					; Get a fourth byte from the result
		adc		r20, zero					; Add carry to r20
		ld		r21, Z					; Get a fifth byte from the result
		adc		r21, zero				; Add carry to r21
		st		Z, r21					; Store seventh byte to memory
		st		-Z, r20					; Store sixth byte to memory
		st		-Z, mpr					; Store fifth byte to memory
		st		-Z, B					; Store fourth byte to memory
		st		-Z, A					; Store third byte to memory
		st		-Z, rhi					; Store second byte to memory
		st		-Z, rlo					; Store first byte to memory
		adiw	ZH:ZL, 1				; Z <= Z + 1			
		dec		iloop					; Decrement counter
		brne	MUL_ILOOP				; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 5				; Z <= Z - 5 (need to shift 5 over for 48 bit MUL)
		adiw	YH:YL, 1				; Y <= Y + 1
		dec		oloop					; Decrement counter
		brne	MUL_OLOOP				; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		r21
		pop		r20
		pop		mpr						; Restore all registers in reverves order
		pop		iloop
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret								; End a function with RET


;***********************************************************
;*	Custom stored data
;*	(feel free to edit these or add others)
;***********************************************************
ONE:		.DB 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
TWO:		.DB	0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
FOURTY:		.DB 0x28, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
HUNDRED:	.DB 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
THOUSAND:	.DB 0x00, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
MIL:		.DB 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00
BIL:		.DB 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00
NEG_ONE:	.DB 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
NEG_TWO:	.DB 0xFE, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF
THOUSANDX:	.DB 0xE8, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
TWENTYFIVE:	.DB 0x19, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00


;***end of your code***end of your code***end of your code***end of your code***end of your code***
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************
;*************************** Do not change anything below this point*******************************

Grading:
		nop					; Check the results in data memory begining at address $0E00 (The TA will set a breakpoint here)
rjmp Grading


;***********************************************************
;*	Stored program data that you cannot change
;***********************************************************

; Contents of program memory will be changed during testing
; The label names (OrbitalRadius, SelectedPlanet, PlanetInfo, MercuryGM, etc) are not changed
; NOTE: All values are provided using the little-endian convention.
OrbitalRadius:	.DB	0x64, 0x19				; the radius that should be used during computations (in kilometers)
											; in this example, the value is 6,500 kilometers
											; the radius will be provided as a 16 bit unsigned value (unless you are
											; completing the extra credit, in which case the radius is an unsigned 24 bit value)

SelectedPlanet:	.DB	0x02, 0x00				; This is how your program knows which GM value should be used.
											; SelectedPlanet is an unsigned 8 bit value that provides you with the
											; index of the planet (and hence, tells you which GM value to use).
											; Note: only the first byte is used. The second byte is just for padding.
											; In this example, the value is 2. If we check the planet at index 2, (from the data below)
											; that corresponds to Earth.
											; if the value was 7, that would correspond to the planet Neptune

PlanetInfo:									; Note that these values will be changed during testing!
MercuryGM:		.DB	0x0E, 0x56, 0x00, 0x00	; Gravitational parameters will be provided as unsigned 32 bit integers (little-endian)
VenusGM:		.DB	0x24, 0xF5, 0x04, 0x00	; the units are in: (km * km * km)/(sec * sec)
EarthGM:		.DB	0x08, 0x15, 0x06, 0x00	; <-- note that this is 398,600
MarsGM:			.DB	0x4E, 0xA7, 0x00, 0x00
JupiterGM:		.DB	0x30, 0x13, 0x8D, 0x07	; A word of advice... treat these like an array, where each element
SaturnGM:		.DB	0xF8, 0xC7, 0x42, 0x02	; occupies 4 bytes of memory.
UranusGM:		.DB	0xD0, 0x68, 0x58, 0x00	; Mercury is at index 0, Venus is at index 1, ...and the final planet is at index 8.
NeptuneGM:		.DB	0x38, 0x4B, 0x68, 0x00
FinalGM:		.DB	0xFF, 0xFF, 0xFF, 0xFF


;***********************************************************
;*	Data Memory Allocation for Results
;*	Your answers need to be stored into these locations (using little-endian representation)
;*	These exact variable names will be used when testing your code!
;***********************************************************
.dseg
.org	$0E00						; data memory allocation for results - Your grader only checks $0E00 - $0E14
Quotient:		.byte 3				; This is the intermediate value that is generated while you are computing the satellite's velocity.
									; It is a 24 bit unsigned value.
Velocity:		.byte 2				; This is where you will store the computed velocity. It is a 16 bit signed number.
									; The velocity value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).
Product:		.byte 7				; This is the intermediate product that is generated while you are computing the orbital period.
Period:			.byte 3				; This is where the orbital period of the satellite will be placed.
									; It is a 24 bit signed value.
									; The period value is normally positive, but it can also be -1 or -2 in case of error
									; (see "Special Cases" in the assignment documentation).

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
