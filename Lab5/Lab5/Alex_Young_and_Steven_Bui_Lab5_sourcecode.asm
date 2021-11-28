;***********************************************************
;*
;*	Alex_Young_and_Steven_Bui_Lab5_sourcecode.asm
;*
;*	This program has functions for 16 bit ADD, 16 bit SUB,
;*  and 24 bit MUL, as well as a compound function
;*
;***********************************************************
;*
;*	 Author: Alex Young and Steven Bui
;*	   Date: 2/4/2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


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
INIT:							; The initialization routine
		; Initialize Stack Pointer
		; TODO					; Init the 2 stack pointer registers
		ldi        mpr, low(RAMEND)
        out        SPL, mpr		; Load SPL with the low byte of RAMEND
        ldi        mpr, high(RAMEND)
        out        SPH, mpr		; Load SPL with the high byte of RAMEND

		clr		zero			; Set the zero register to zero, maintain
								; these semantics, meaning, don't
								; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program
		; Setup the ADD16 function direct test

				; Move values 0xFCBA and 0xFFFF in program memory to data memory
				; memory locations where ADD16 will get its inputs from
				; (see "Data Memory Allocation" section below)
				ldi		ZL, low(ADD16_Operand1<<1)		; ADD16_Operand1 is 0xFCBA, load low byte into Z register
				ldi		ZH, high(ADD16_Operand1<<1)		; Load high byte into Z register
				ldi		XL, low(ADD16_OP1)				; ADD16_OP1 points to the address of operand 1 for addition, load low byte into X register
				ldi		XH, high(ADD16_OP1)				; Load high byte into X register
				lpm		mpr, Z+							; Load 0xBA into mpr
				st		X+, mpr							; Store 0xBA into data memory
				lpm		mpr, Z							; Load 0xFC into mpr
				st		X, mpr							; Store 0xFC into data memory

				ldi		ZL, low(ADD16_Operand2<<1)		; ADD16_Operand2 is 0xFFFF, load low byte into Z register
				ldi		ZH, high(ADD16_Operand2<<1)		; Load high byte into Z register
				ldi		YL, low(ADD16_OP2)				; ADD16_OP2 points to the address of operand 2 for addition, load low byte into Y register
				ldi		YH, high(ADD16_OP2)				; Load high byte into Y register
				lpm		mpr, Z+							; Load 0xFF into mpr
				st		Y+, mpr							; Store 0xFF into data memory
				lpm		mpr, Z							; Load 0xFF into mpr
				st		Y, mpr							; Store 0xFF into data memory

                nop ; Check load ADD16 operands (Set Break point here #1)  
				; Call ADD16 function to test its correctness
				; (calculate FCBA + FFFF)
				rcall	ADD16

                nop ; Check ADD16 result (Set Break point here #2)
				; Observe result in Memory window

		; Setup the SUB16 function direct test

				; Move values 0xFCB9 and 0xE420 in program memory to data memory
				; memory locations where SUB16 will get its inputs from
				ldi		ZL, low(SUB16_Operand1<<1)		; SUB16_Operand1 is 0xE420, load low byte into Z register
				ldi		ZH, high(SUB16_Operand1<<1)		; Load high byte into Z register
				ldi		XL, low(SUB16_OP1)				; SUB16_OP1 points to the address of operand 1 for subtraction, load low byte into X register
				ldi		XH, high(SUB16_OP1)				; Load high byte into X register
				lpm		mpr, Z+							; Load 0x20 into mpr
				st		X+, mpr							; Store 0x20 into data memory
				lpm		mpr, Z							; Load 0xE4 into mpr
				st		X, mpr							; Store 0xE4 into data memory

				ldi		ZL, low(SUB16_Operand2<<1)		; SUB16_Operand2 is 0xFCB9, load low byte into Z register
				ldi		ZH, high(SUB16_Operand2<<1)		; Load high byte into Z register
				ldi		YL, low(SUB16_OP2)				; SUB16_OP2 points to the address of operand 2 for subtraction, load low byte into Y register
				ldi		YH, high(SUB16_OP2)				; Load high byte into Y register
				lpm		mpr, Z+							; Load 0xB9 into mpr
				st		Y+, mpr							; Store 0xB9 into data memory
				lpm		mpr, Z							; Load 0xFC into mpr
				st		Y, mpr							; Store 0xFC into data memory

                nop ; Check load SUB16 operands (Set Break point here #3)  
				; Call SUB16 function to test its correctness
				; (calculate FCB9 - E420)
				rcall SUB16

                nop ; Check SUB16 result (Set Break point here #4)
				; Observe result in Memory window

		; Setup the MUL24 function direct test

				; Move values 0xFFFFFF and 0xFFFFFF in program memory to data memory  
				; memory locations where MUL24 will get its inputs from
				ldi        ZL, low(MUL24_Operand1B<<1)		; MUL24_Operand1B is 0xFF, load low byte into Z register
                ldi        ZH, high(MUL24_Operand1B<<1)		; Load high byte into Z register
                ldi        XL, low(MUL24_OP1)				; MUL24_OP1 points to the address of operand 1 for multiplication, load low byte into X register
                ldi        XH, high(MUL24_OP1)				; Load high byte into X register
                lpm        mpr, Z+							; Load 0xFF into mpr
                st         X+, mpr							; Store 0xFF into data memory
                ldi        ZL, low(MUL24_Operand1A<<1)		; MUL24_Operand1A is 0xFFFF, load low byte into Z register
                ldi        ZH, high(MUL24_Operand1A<<1)		; Load high byte into Z register
                lpm        mpr, Z+							; Load 0xFF into mpr
                st         X+, mpr							; Store 0xFF into data memory
                lpm        mpr, Z							; Load 0xFF into mpr
                st         X, mpr							; Store 0xFF into data memory

                ldi        ZL, low(MUL24_Operand2B<<1)		; MUL24_Operand2B is 0xFF, load low byte into Z register
                ldi        ZH, high(MUL24_Operand2B<<1)		; Load high byte into Z register
                ldi        YL, low(MUL24_OP2)				; MUL24_OP2 points to the address of operand 2 for multiplication, load low byte into Y register
                ldi        YH, high(MUL24_OP1)				; Load high byte into Y register
                lpm        mpr, Z+							; Load 0xFF into mpr
                st         Y+, mpr							; Store 0xFF into data memory
                ldi        ZL, low(MUL24_Operand2A<<1)		; MUL24_Operand2A is 0xFFFF, load low byte into Z register
                ldi        ZH, high(MUL24_Operand2A<<1)		; Load high byte into Z register
                lpm        mpr, Z+							; Load 0xFF into mpr
                st         Y+, mpr							; Store 0xFF into data memory
                lpm        mpr, Z							; Load 0xFF into mpr
                st         Y, mpr							; Store 0xFF into data memory

                nop ; Check load MUL24 operands (Set Break point here #5)  
				; Call MUL24 function to test its correctness
				; (calculate FFFFFF * FFFFFF)
				rcall	MUL24

                nop ; Check MUL24 result (Set Break point here #6)
				; Observe result in Memory window

                nop ; Check load COMPOUND operands (Set Break point here #7)  
				; Call the COMPOUND function
				rcall	COMPOUND

                nop ; Check COMPUND result (Set Break point here #8)
				; Observe final result in Memory window

DONE:	rjmp	DONE			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;		where the high byte of the result contains the carry
;		out bit.
;-----------------------------------------------------------
ADD16:

		clr		zero					; Maintain zero semantics
		ldi		XL, low(ADD16_Result)	; Clear low byte of result data memory
		ldi		XH, high(ADD16_Result)	; Clear high byte of result data memory
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X, zero					; Set data memory to 0

		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)		; Load low byte of address
		ldi		XH, high(ADD16_OP1)		; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(ADD16_OP2)		; Load low byte of address
		ldi		YH, high(ADD16_OP2)		; Load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(ADD16_Result)	; Load low byte of address
		ldi		ZH, high(ADD16_Result)	; Load high byte of address

		; Execute the function here
        ld      r20, X+					; Load register 20 with low byte of op1
        ld      r21, Y+					; Load register 21 with low byte of op2
        add     r21, r20				; Add together righthandside of equation
        st      Z+, r21					; Store result
        ld      r20, X					; Load register 20 with high byte of op1
        ld      r21, Y					; Load register 21 with high byte of op2
        adc		r21, r20				; Add together lefthandside of equation with carry
        st		Z+, r21					; Store result
        brcc	EXIT					; If no carry, return to caller
        st		Z, XH					; Store carry result if it exists

EXIT:
        ret								; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;		result.
;-----------------------------------------------------------
SUB16:

		clr		zero					; Maintain zero semantics
		ldi		XL, low(SUB16_Result)	; Clear low byte of result data memory
		ldi		XH, high(SUB16_Result)	; Clear high byte of result data memory
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X, zero					; Set data memory to 0

		; Load beginning address of first operand into X
		ldi		XL, low(SUB16_OP1)		; Load low byte of address
		ldi		XH, high(SUB16_OP1)		; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(SUB16_OP2)		; Load low byte of address
		ldi		YH, high(SUB16_OP2)		; Load high byte of address

		; Load beginning address of result into Z
		ldi		ZL, low(SUB16_Result)	; Load low byte of address
		ldi		ZH, high(SUB16_Result)	; Load high byte of address

		; Execute the function here
        ld      r20, X+					; Load register 20 with low byte of op1
        ld      r21, Y+					; Load register 21 with low byte of op2
        sub     r21, r20				; Subtract righthandside of equation
        st      Z+, r21					; Store result
        ld      r20, X					; Load register 20 with high byte of op1
        ld      r21, Y					; Load register 21 with high byte of op2
        sbc		r21, r20				; Subtract lefthandside of equation with carry
        st		Z+, r21					; Store result
        brcc	EXIT2					; If no carry, return to caller
        st		Z, XH					; Store carry result if it exists

EXIT2:
        ret								; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit 
;		result.
;		A - Operand A is gathered from address $0101:$0100
;		B - Operand B is gathered from address $0103:$0102
;-----------------------------------------------------------
MUL24:
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

		clr		zero					; Maintain zero semantics
		ldi		XL, low(MUL24_Result)	; Clear low byte of result data memory
		ldi		XH, high(MUL24_Result)	; Clear high byte of result data memory
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X+, zero				; Set data memory to 0
		st		X, zero					; Set data memory to 0

		; Set Y to beginning address of B
		ldi		YL, low(MUL24_OP2)		; Load low byte
		ldi		YH, high(MUL24_OP2)		; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(MUL24_Result)	; Load low byte
		ldi		ZH, high(MUL24_Result)	; Load high byte

		; Begin outer for loop
		ldi		oloop, 3				; Load counter for 3 loops
MUL24_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(MUL24_OP1)		; Load low byte
		ldi		XH, high(MUL24_OP1)		; Load high byte

		; Begin inner for loop
		ldi		iloop, 3				; Load counter for 3 loops
MUL24_ILOOP:
		ld		A, X+					; Get byte of A operand
		ld		B, Y					; Get byte of B operand
		mul		A,B						; Multiply A and B
		ld		A, Z+					; Get a result byte from memory
		ld		B, Z+					; Get the next result byte from memory
		add		rlo, A					; rlo <= rlo + A
		adc		rhi, B					; rhi <= rhi + B + carry
		ld		A, Z+					; Get a third byte from the result
		adc		A, zero					; Add carry to A
		ld		B, Z					; Get a fourth byte from the result
		adc		B, zero					; Add carry to B
		st		Z, B					; Store fourth byte to memory
		st		-Z, A					; Store third byte to memory
		st		-Z, rhi					; Store second byte to memory
		st		-Z, rlo					; Store first byte to memory
		adiw	ZH:ZL, 1				; Z <= Z + 1			
		dec		iloop					; Decrement counter
		brne	MUL24_ILOOP				; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 2				; Z <= Z - 2 (need to shift two over for 24 bit MUL)
		adiw	YH:YL, 1				; Y <= Y + 1
		dec		oloop					; Decrement counter
		brne	MUL24_OLOOP				; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop					; Restore all registers in reverves order
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

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((D - E) + F)^2
;		by making use of SUB16, ADD16, and MUL24.
;
;		D, E, and F are declared in program memory, and must
;		be moved into data memory for use as input operands.
;
;		All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:

		; Setup SUB16 with operands D and E
		; Perform subtraction to calculate D - E
		ldi		ZL, low(OperandE<<1)		; Load operand E into Z register, load low byte into Z register
		ldi		ZH, high(OperandE<<1)		; Load high byte into Z register
		ldi		XL, low(SUB16_OP1)			; Load subtraction operand address into X register, load low byte into X register
		ldi		XH, high(SUB16_OP1)			; Load high byte into X register
		lpm		mpr, Z+						; Load low byte of operand E stored in prog memory to mpr
		st		X+, mpr						; Store low byte of operand E to address in X register
		lpm		mpr, Z						; Load high byte of operand E stored in prog memory to mpr
		st		X, mpr						; Store high byte of operand E to address in X register

		ldi		ZL, low(OperandD<<1)		; Load operand D into Z register, load low byte into Z register
		ldi		ZH, high(OperandD<<1)		; Load high byte into Z register
		ldi		YL, low(SUB16_OP2)			; Load subtraction operand address into Y register, load low byte into Y register
		ldi		YH, high(SUB16_OP2)			; Load high byte into Y register
		lpm		mpr, Z+						; Load low byte of operand D stored in prog memory to mpr
		st		Y+, mpr						; Store low byte of operand D to address in Y register
		lpm		mpr, Z						; Load high byte of operand D stored in prog memory to mpr
		st		Y, mpr						; Store high byte of operand D to address in Y register

		rcall SUB16							; calculate (D - E)

		
		; Setup the ADD16 function with SUB16 result and operand F
		; Perform addition next to calculate (D - E) + F
		ldi		ZL, low(SUB16_Result)		; Load low byte of subtraction result of D - E into ZL register
		ldi		ZH, high(SUB16_Result)		; Load high byte of subtraction result of D - E into ZH register
		ldi		XL, low(ADD16_OP1)			; Load addition operand address into X register, load low byte into X register
		ldi		XH, high(ADD16_OP1)			; Load high byte into X register
		ld		mpr, Z+						; Load low byte of (D - E) result stored in prog memory to mpr
		st		X+, mpr						; Store low byte of (D - E) result to address in X register
		ld		mpr, Z						; Load high byte of (D - E) result stored in prog memory to mpr
		st		X, mpr						; Store high byte of (D - E) result to address in X register

		ldi		ZL, low(OperandF<<1)		; Load operand F into Z register, load low byte into Z register
		ldi		ZH, high(OperandF<<1)		; Load high byte into Z register
		ldi		YL, low(ADD16_OP2)			; Load addition operand address into Y register, load low byte into Y register
		ldi		YH, high(ADD16_OP2)			; Load high byte into Y register
		lpm		mpr, Z+						; Load low byte of operand F stored in prog memory to mpr
		st		Y+, mpr						; Store low byte of operand F to address in Y register
		lpm		mpr, Z						; Load high byte of operand F stored in prog memory to mpr
		st		Y, mpr						; Store high byte of operand F to address in Y register

		rcall	ADD16						; Calculate (D - E) + F

		; Setup the MUL24 function with ADD16 result as both operands
		; Perform multiplication to calculate ((D - E) + F)^2
		ldi		ZL, low(ADD16_Result)		; Load low byte of addition result of ((D - E) + F) into ZL register
		ldi		ZH, high(ADD16_Result)		; Load high byte of addition result of ((D - E) + F) into ZH register
		ldi		XL, low(MUL24_OP1)			; Load multiplication operand address into X register, load low byte into X register
		ldi		XH, high(MUL24_OP1)			; Load high byte into X register
		ld		mpr, Z+						; Load low byte of ((D - E) + F) result stored in prog memory to mpr
		st		X+, mpr						; Store low byte of ((D - E) + F) result to address in X register
		ld		mpr, Z+						; Load high byte of ((D - E) + F) result stored in prog memory to mpr
		st		X+, mpr						; Store high byte of ((D - E) + F) result to address in X register
		ld		mpr, Z						; Load byte 3 of ((D - E) + F) result stored in prog memory to mpr
		st		X, mpr						; Store byte 3 of ((D - E) + F) result to address in X register

		ldi		ZL, low(ADD16_Result)		; Load low byte of addition result of ((D - E) + F) into ZL register
		ldi		ZH, high(ADD16_Result)		; Load high byte of addition result of ((D - E) + F) into ZH register
		ldi		YL, low(MUL24_OP2)			; Load multiplication operand address into X register, load low byte into Y register
		ldi		YH, high(MUL24_OP2)			; Load high byte into Y register
		ld		mpr, Z+						; Load low byte of ((D - E) + F) result stored in prog memory to mpr
		st		Y+, mpr						; Store low byte of ((D - E) + F) result to address in Y register
		ld		mpr, Z+						; Load high byte of ((D - E) + F) result stored in prog memory to mpr
		st		Y+, mpr						; Store high byte of ((D - E) + F) result to address in Y register
		ld		mpr, Z						; Load byte 3 of ((D - E) + F) result stored in prog memory to mpr
		st		Y, mpr						; Store byte 3 of ((D - E) + F) result to address in Y register

		rcall	MUL24						; Calculate ((D - E) + F)^2

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

; ADD16 operands
ADD16_Operand1:
	.DW	0xFCBA				; test value 1 for operand ADD16 function
ADD16_Operand2:
	.DW	0xFFFF				; test value 2 for operand ADD16 function
; SUB16 operands
SUB16_Operand1:
	.DW 0xE420				; test value 1 for operand SUB16 function
SUB16_Operand2:
	.DW 0xFCB9				; test value 2 for operand SUB16 function
; MUL24 operands
MUL24_Operand1A:			; if we were to mul ABCDEF this would be formated ABCD-EF
    .DW 0xFFFF				; test value 1A for operand MUL24 function
MUL24_Operand1B:
    .DW 0x00FF				; test value 1B for operand MUL24 function
MUL24_Operand2A:
    .DW 0xFFFF				; test value 2A for operand MUL24 function
MUL24_Operand2B:
    .DW 0x00FF				; test value 2B for operand MUL24 function
; Compoud operands
OperandD:
	.DW	0xFCBA				; test value for operand D
OperandE:
	.DW	0x2019				; test value for operand E
OperandF:
	.DW	0x21BB				; test value for operand F

;***********************************************************
;*	Data Memory Allocation
;***********************************************************

.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

.org	$0114				; data memory allocation for operands
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of SUB16

.org	$0124				; data memory allocation for results
SUB16_Result:
		.byte 3				; allocate three bytes for SUB16 result

.org	$0118				; data memory allocation for operands
MUL24_OP1:
		.byte 3				; allocate three bytes for first operand of MUL24
MUL24_OP2:
		.byte 3				; allocate three bytes for second operand of MUL24

.org	$0128				; data memory allocation for results
MUL24_Result:
		.byte 6				; allocate six bytes for MUL26 result

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program