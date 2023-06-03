IDEAL 
MODEL SMALL
STACK 100h 
DATASEG 

; --------------------------   general data    --------------------------


	direction dw ? ; 1 - up, 2 - right, 3 - down, 4 - left 
	pointx	  dw ?
	pointy	  dw ?
	point1x   dw 100;175;50;75		    ;100
	point1y   dw 50;100;50;100      ;50
	point2x   dw 200;225;100;125		  ;150;250
	point2y   dw 50;125;50;125  		;150;180
	point3x	  dw 150;200;50;100		  ;200;20
	point3y   dw 100;175;100;75		    ;100;150
	;              +0     +2      +4    +6      +8      +10
	;              Δx   	Δy    	sx  	sy    	error 	e2
	variables dw   0,	  	0,	  	1,		1,	 		0,			0
	result 	  dw ?
	;                 newx, newy
	rotationResult dw ?,     ?
	;                              invslope1   insvlope2
	drawFilledVariables     dw 	    	0, 		      0  
	
	currentColor db 42 ; the current color that is selected

; ----------------------------------------------------------------

; --------------------   points input data    --------------------
	
	isTPressed dw 0     ; Triangle input press flag 

	pointsInputedAmount dw 0 ; current amount of inputed points
  
	pointsXCoordinatesInputedBuffer dw 3 dup(0)
	
	pointsYCoordinatesInputedBuffer dw 3 dup (0)

; ----------------------------------------------------------------

; -------------------   saved triangles data    -------------------

	trianglesAmount dw 2
	
	trianglesXCoordinates dw 100, 200, 150, 999, 75, 125, 100, 999
	dw 132 dup (0) ; format: x_1_1, x_1_2, x_1_3, 999, x_2_1 ...

	trianglesYCoordinates dw 50, 50, 100, 999, 100, 125, 75, 999
	dw 132 dup (0) ; format: y_1_1, y_1_2, y_1_3, 999, y_2_1 ...
	
	trianglesDegreeRotationValues dw 30, 30
	dw 11 dup (30)
	
	trianglesColors db 10, 12
	db 11 dup (0)
	
	trianglesClearColors db 11 dup (0)

; ----------------------------------------------------------------

;---------------------------------------------------------------------------------------------------------------------------------
;----------------------------- SINES 'TABLE' FROM 0° TO 360° MULTIPLIED BY 102 ---------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------
sinesX10  dw 0
  dw 0
  dw 0
  dw 1
  dw 1
  dw 1
  dw 1
  dw 1
  dw 1
  dw 2
  dw 2
  dw 2
  dw 2
  dw 2
  dw 2
  dw 3
  dw 3
  dw 3
  dw 3
  dw 3
  dw 3
  dw 4
  dw 4
  dw 4
  dw 4
  dw 4
  dw 4
  dw 5
  dw 5
  dw 5
  dw 5
  dw 5
  dw 5
  dw 5
  dw 6
  dw 6
  dw 6
  dw 6
  dw 6
  dw 6
  dw 6
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 10
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 9
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 8
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 7
  dw 6
  dw 6
  dw 6
  dw 6
  dw 6
  dw 6
  dw 6
  dw 5
  dw 5
  dw 5
  dw 5
  dw 5
  dw 5
  dw 5
  dw 4
  dw 4
  dw 4
  dw 4
  dw 4
  dw 4
  dw 3
  dw 3
  dw 3
  dw 3
  dw 3
  dw 3
  dw 2
  dw 2
  dw 2
  dw 2
  dw 2
  dw 2
  dw 1
  dw 1
  dw 1
  dw 1
  dw 1
  dw 1
  dw 0
  dw 0
  dw 0
  dw 0
  dw 0
  dw -1
  dw -1
  dw -1
  dw -1
  dw -1
  dw -1
  dw -2
  dw -2
  dw -2
  dw -2
  dw -2
  dw -2
  dw -3
  dw -3
  dw -3
  dw -3
  dw -3
  dw -3
  dw -4
  dw -4
  dw -4
  dw -4
  dw -4
  dw -4
  dw -5
  dw -5
  dw -5
  dw -5
  dw -5
  dw -5
  dw -5
  dw -6
  dw -6
  dw -6
  dw -6
  dw -6
  dw -6
  dw -6
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -10
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -9
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -8
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -7
  dw -6
  dw -6
  dw -6
  dw -6
  dw -6
  dw -6
  dw -6
  dw -5
  dw -5
  dw -5
  dw -5
  dw -5
  dw -5
  dw -5
  dw -4
  dw -4
  dw -4
  dw -4
  dw -4
  dw -4
  dw -3
  dw -3
  dw -3
  dw -3
  dw -3
  dw -3
  dw -2
  dw -2
  dw -2
  dw -2
  dw -2
  dw -2
  dw -1
  dw -1
  dw -1
  dw -1
  dw -1
  dw -1
  dw 0
  dw 0
  dw 0
;---------------------------------------------------------------------------------------------------------------------------------
;----------------------------------------------------- SINES TABLE END -----------------------------------------------------------
;---------------------------------------------------------------------------------------------------------------------------------	

CODESEG

GRAPHICAL_SCREEN_VALUE equ 0A000h 

SCREEN_WIDTH 		equ		320
SCREEN_HEIGHT 	equ 	200  

GREEN_PIXEL			equ 	10
RED_PIXEL				equ 	4
BLACK_PIXEL 		equ		0

DECREMENT				equ   1 
INCREMENT				equ 	1

RIGHT_ANGLE 		equ 	90 

ANGLE_DIV				equ 	10

ESCAPE_KEY			equ		27 

UP_DIRECTION		equ		1
RIGHT_DIRECTION	equ		2
DOWN_DIRECTION	equ		3
LEFT_DIRECTION	equ		4

VALUES_SEPARATOR equ 999

CLOCK equ es:6Ch

; NOTE - pointer = offset in the documnetation. 

proc delay 
	; function that delays the program using nested loop that does nothing. 
	push cx 
	mov cx, 10
	outerDelayloop:
		push cx 
		mov cx, 0ffffh
		innerDelayLoop: loop innerDelayLoop
	pop cx 
	loop outerDelayloop	
	pop cx 
	ret 
endp delay

proc delay2
	; function that delays the program using nested loop that does nothing. 
	push cx 
	mov cx, 80h
	outerDelayloop2:
		push cx 
		mov cx, 0ffffh
		innerDelayLoop2: loop innerDelayLoop2
	pop cx 
	loop outerDelayloop2
	pop cx 
	ret 
endp delay2

proc divWithRound
  ; Function that does division with round for signed values. 
  push bp 
  mov bp, sp 
  push ax 
  push bx 
  push cx 
  push dx 
  push si 
  push di 
  mov di, 0 ; di will determine if the final result should be negative or positive.

  mov si, [bp+4] ; the offset of 'result' variable in the DATASEG
  
  mov ax, [bp+8] ; dividend value 
  mov bx, [bp+6] ; divisor value 

  ; check if both ax and bx positive
  cmp ax, 0 
  jl checkBxNegative ; ax negative 
  jmp checkOnlyBxNegative
checkBxNegative:
  cmp bx, 0 
  jg diFlagOn ; bx positive
  jmp diFlagOff
checkOnlyBxNegative:
  cmp bx, 0
  jge diFlagOff

diFlagOn:
  mov di, 1
diFlagOff:
  
; calc absolute values of ax and bx:
  push bx
  xor bx, bx
  mov bx, ax 
  sar bx, 15 ; 16 - 1 = 15 bits, shift the sign bit of BX into all the bits
	xor ax, bx ; toggle all the bits of AX if it's negative
	sub ax, bx ; add the sign bit back if it was toggled 
  pop bx 
  
  push ax
  xor ax, ax 
  mov ax, bx 
  sar ax, 15 ; 16 - 1 = 15 bits, shift the sign bit of AX into all the bits
	xor bx, ax ; toggle all the bits of BX if it's negative
	sub bx, ax ; add the sign bit back if it was toggled 
  pop ax 
  
  xor dx, dx ; clear the previous values in dx 
  div bx  

  push dx ; save the value of dx 
  mov cx, bx
  mov dx, cx 
  and dx, 1
  cmp cx, dx  
  shr cx, 1
  je oddDivisor 
  jmp evenDivisor
  
oddDivisor:
  inc cx

evenDivisor:
  pop dx

  cmp cx, dx 
  jae skipRoundResult
  inc ax

skipRoundResult:
  cmp di, 1 
  jne skipNegateResult

; negate ax
  neg ax

skipNegateResult:
  mov [si], ax 

  pop di 
  pop si 
  pop dx 
  pop cx 
  pop bx 
  pop ax 
  pop bp
  ret 6 
endp divWithRound

proc sinX10
	; sine function that gets an integer degree value and returns it approximate sine integer value result * 10.  
	; PARAMETERS:
	; [bp+4] - pointer to the 'sinesx10' array in the DATASEG
	; [bp+6] - the degree value parameter
	; [bp+8] - pointer to the 'result' variable in the DATASEG
	; RETURNS:
	; approximate sine integer value result * 10 that will be stored in 'result' variable in the DATASEG
	
	push bp 
	mov bp, sp
	
	push si ; pointer to the 'sinesx10' Look-Up Table in the DATASEG 
	push di ; the degree value parameter
	push bx ; pointer to the 'result' variable  in the DATASEG
	push ax ; a register to use    
	
	xor ax, ax 
	mov si, [bp+4] ; pointer to the 'sinesx10' Look-Up Table in the DATASEG 
	mov di, [bp+6] ; the degree value 
	mov bx, [bp+8] ; pointer to the 'result' variable  in the DATASEG

	mov ax, 1

	cmp di, 0
	jl negSin
	cmp di, 0 
	jne incSin
	jmp skipIncSin

negSin:
	neg di 
	mov ax, -1
incSin:
	inc di 
skipIncSin:
	shl di, 1 ; convert byte offset to word offset 
	add si, di

	mov di, [si] ; ax = sin(di) * 10

	cmp ax, -1 
	je negSinResult
	jmp skipNegSinResult

negSinResult:
	neg di 
skipNegSinResult:

	mov [bx], di
	
	pop ax 
	pop bx 
	pop di
	pop si
	pop bp
	ret 6
endp sinX10 

proc cosX10
	; cosine function that gets an integer degree value and returns it approximate cosine integer value result * 10.  
	; PARAMETERS:
	; [bp+4] - pointer to the 'sinesx10' array in the DATASEG
	; [bp+6] - the degree value parameter
	; [bp+8] - pointer to the 'result' variable in the DATASEG
	; RETURNS:
	; approximate cosine integer value result * 10 that will be stored in 'result' variable in the DATASEG
	
	push bp 
	mov bp, sp
	
	push si ; pointer to the 'sinesx10' array in the DATASEG 
	push di ; the degree value parameter
	push bx ; pointer to the 'result' variable in the DATASEG
	push ax ; a register to use
	
	xor ax, ax
	mov si, [bp+4]
	mov di, [bp+6] 
	mov bx, [bp+8]

	; cos(x) = sin(90-x)
	mov ax, di
	mov di, RIGHT_ANGLE
	sub di, ax
	
	push bx 
	push di 
	push si
	call sinX10

	pop ax
	pop bx
	pop di 
	pop si
	pop bp
	ret 6
endp cosX10

proc clearGraphicScreen
	push bp 
	mov bp, sp 
	push es 
	push bx
	push ax
	mov es, [bp+4] ; the offset of the graphical screen 
	xor ax, ax 
	xor bx, bx 
	clearGraphicScreenLoop:
		mov [es:bx], ax 
		inc bx
		cmp bx, (SCREEN_HEIGHT * SCREEN_WIDTH)-DECREMENT
	jne clearGraphicScreenLoop
	pop ax 
	pop bx
	pop es 
	pop bp 
	ret 2 
endp clearGraphicScreen

proc pointCoordinatesToPixelCoordinate
	; this procedure gets an (x,y) coordinate and converts it to coordinate on the graphic screen
	; and then 'returns' it to the result variable in the DATASEG 
	push bp 
	mov bp, sp 
	push bx ; result coordinates
	push dx ; point x coordinates
	push cx ; point y coordinates
	push ax ; the final result 

	mov bx, [bp+8] ; result
	mov dx, [bp+6] ; x 
	mov cx, [bp+4] ; y
	
	mov ax, dx
	
	multLoopPointToCoordinate:
	add ax, SCREEN_WIDTH
	loop multLoopPointToCoordinate
	
	mov [bx], ax 
	
	pop ax 
	pop cx 
	pop dx
	pop bx 
	pop bp 
	ret 6 
endp pointCoordinatesToPixelCoordinate

proc drawLine
; this procedure gets 2 (x,y) coordinates as input and draws a line between them 
	push bp
	mov bp, sp 
	push ax ; x1 
	push bx ; y1 
	push cx ; x2 
	push dx ; y2 
	push es 
	push si 
	push di 	 
	
	; hide mouse cursor 
	push ax
	mov ax, 2h
	int 33h 
	pop ax 
	
	mov es, [bp+16] ; graphic screen pointer
	mov ax, [bp+14] ; x1 
	mov bx, [bp+12] ; y1 
	mov cx, [bp+10] ; x2
	mov dx, [bp+8]  ; y2
	mov si, [bp+6]  ; the offset of the 'result' variable in the DATASEG
	mov di, [bp+4]  ; the offset of the 'variables' array in the DATASEG 

; --- Δx: ---
	; Δx = abs(x2 - x1)
	mov [di], cx 
	sub [di], ax 
	
	; abs the Δx:
	push ax 
	push bx 
	xor bx, bx ; bx is zero now
	mov ax, [di]
	mov bx, [di]
	sar bx, 15 ; 16 - 1 = 15 bits, shift the sign bit of BX into all the bits
	xor ax, bx ; toggle all the bits of AX if it's negative
	sub ax, bx ; add the sign bit back if it was toggled
	mov [di], ax
	pop bx 
	pop ax 
	
; --- sx: ---
	push ax ; save the value of ax 
	mov ax, 1
	mov [di+4], ax ; set sx = 1 as default
	pop ax 
	
	cmp ax, cx 
	jge negSx
	jmp skipNegSx

negSx:
	push ax ; save the value of ax 
	mov ax, -1
	mov [di+4], ax ; sx = -1
	pop ax 

skipNegSx:
	
; --- Δy: ---
	; Δy = -abs(y2 - y1) 
	mov [di+2], dx 
	sub [di+2], bx 
	
	; abs Δy
	push ax
	push bx
	mov ax, [di+2]
	mov bx, [di+2]
	sar bx, 15 ; 16 - 1 = 15 bits, shift the sign bit of BX into all the bits
	xor ax, bx ; toggle all the bits of AX if it's negative
	sub ax, bx ; add the sign bit back if it was toggled
	mov [di+2], ax
	pop bx 
	pop ax
	
	; negate abs(Δy) to -abs(Δy)
	push ax 
	mov ax, [di+2] 
	neg ax
	mov [di+2], ax
	pop ax	
	
; --- sy: ---
	push ax ; save the value of ax  
	mov ax, 1
	mov [di+6], ax ; set sy = 1 as default 
	pop ax 
	
	cmp bx, dx 
	jge negSy
	jmp skipNegSy

negSy:
	push ax ; save the value of ax
	mov ax, -1
	mov [di+6], ax ; sy = -1
	pop ax 

skipNegSy:

; --- error: ---
	push ax ; save the value of ax 
	mov ax, [di] 	; ax = Δx
	mov [di+8], ax  ; error = Δx
	mov ax, [di+2]  ; ax = Δy  
	add [di+8], ax  ; final result -> error = Δx + Δy 
	pop ax 

; --- Line Drawing Loop: ---
lineDrawLoop:

	; calculate the pixel coordinate on the screen:
	push si ; offset of 'result' var
	push ax ; current pixel x
	push bx ; current pixel y
	call pointCoordinatesToPixelCoordinate
	
	; put pixel on the screen:
	push ax 
	push bx 
	mov al, [byte ptr bp+18] ; color
	mov bx, [si] 
	mov [byte ptr es:bx], al 
	pop bx 
	pop ax 
	
	; is x1 == x2 && y1 == y2? :
	cmp ax, cx 
	je isY1equalsY2
	jmp skipIsY1equalsY2
	
isY1equalsY2:
	cmp bx, dx 
	je endDrawLineLoop

skipIsY1equalsY2:
	
; --- e2 update: --- 
	; e2 = 2*error
	push ax ; save the value of ax 
	mov ax, [di+8]
	add ax, [di+8] 
	mov [di+10], ax 
	pop ax 
	
; --- update x: ---
	push bx ; save the value of bx 
	mov bx, [di+10] ; bx = e2  
	cmp bx, [di+2]  ; e2 >= Δy
	jge conditionsWithDeltaYAndE2
	jmp skipConditionsWithDeltaYAndE2

conditionsWithDeltaYAndE2:
	pop bx
	cmp ax, cx  
	je endDrawLineLoop
	push bx ; save the value of bx
	mov bx, [di+2] ; bx = Δy
	add [di+8], bx ; error += Δy
	pop bx 
	add ax, [di+4] ; x1 += sx 
	push bx
	
skipConditionsWithDeltaYAndE2:
	pop bx 
	
;--- update y: ---

	push ax         ; save the value of ax 
	mov ax, [di+10] ; ax = e2
	cmp ax, [di]    ; e2 <= Δx  
	jle conditionsWithDeltaXAndE2
	jmp skipConditionsWithDeltaXAndE2

conditionsWithDeltaXAndE2:
	pop ax 
	cmp bx, dx
	je endDrawLineLoop
	push ax
	mov ax, [di] 	; ax = Δx
	add [di+8], ax  ; error += Δx
	add bx, [di+6]  ; y1 += sy 
	pop ax
	push ax

skipConditionsWithDeltaXAndE2:
	pop ax
	
jmp lineDrawLoop 
	
endDrawLineLoop:
	mov ax, 1h ; show back the mouse 
	int 33h 
	
	pop di 
	pop si 
	pop es 
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	pop bp 
	ret 16
endp drawLine

proc drawTriangle
	push bp
	mov bp, sp

	push es ; register for the graphic screen 
	push si ; the offset of the 'result' variable in the DATASEG
	push di ; the offset of the 'variables' variable in the DATASEG
	push ax ; register to store point coordinates 
	push bx ; register to store point coordinates 
	push cx ; register to store point coordinates 
	push dx ; register to store point coordinates 
	
	mov es, [bp+20]
	mov si, [bp+6]
	mov di, [bp+4]
	mov ax, [bp+18] ; x1
	mov bx, [bp+16] ; y1
	mov cx, [bp+14] ; x2
	mov dx, [bp+12] ; y2
	
; line between point1 to point2
	push [bp+22] ; color
	push es 
	push ax
	push bx 
	push cx 
	push dx
	push si 
	push di 
	call drawLine
	
; line between point1 to point3
	push [bp+22] ; color
	mov cx, [bp+10] ; x3
	mov dx, [bp+8] ; y3
	push es 
	push ax
	push bx 
	push cx 
	push dx
	push si 
	push di 
	call drawLine

; line between point2 to point3:
	push [bp+22] ; color
	mov ax, [bp+14] ; x2
	mov bx, [bp+12] ; y2
	push es 
	push ax
	push bx 
	push cx 
	push dx
	push si 
	push di 
	call drawLine

	pop dx
	pop cx
	pop bx
	pop ax 
	pop di 
	pop si
	pop es 
	pop bp 
	ret 20
endp drawTriangle

proc rotatePointAroundOtherPoint
	push bp 
	mov bp, sp
	
; NOTE: point2(x2, y2) is rotated arround point1(x1, y1)

	push si ; pointer to the 'rotationResult' variable in the DATASEG
	push di ; pointer to the 'result' varianle in the DATASEG
	push ax  
	push bx 
	push cx  
	push dx 
	push es
	
	mov es, [bp+18] ; offset of 'sinesx10'
	mov si, [bp+16] ; offset of 'rotationResult'  
	mov di, [bp+14] ; offset of 'result'
	mov ax, [bp+10] ; x1
	mov bx, [bp+6]  ; x2
	mov cx, [bp+12] ; angle
	
; x' calculation, where x' is the x2 point coordinate after rotatin by a given angle
	; x' = cos(angle) * (x2 - x1) - sin(angle) * (y2 - y1) + x1
	mov [si], ax ; x' = x1
	
	; calculate cos(angle) * (x2 - x1):
	xor dx, dx
	; dx = x2 - x1
	mov dx, bx 
	sub dx, ax
	push di 
	push cx 
	push es
	call cosX10
	mov ax, [di] ; ax = cos(angle) * 10
	push cx      ; save cx 
	mov cx, dx  
	xor dx, dx
	imul cx       ; ax = cos(angle) * 10 * (x2 - x1)
	mov cx, ANGLE_DIV
	xor dx, dx 
	cwd
	idiv cx
	add [si], ax ; x' += cos(angle) * (x2 - x1)
	pop cx 

	; calculate sin(angle) * (y2 - y1):
	xor dx, dx
	mov ax, [bp+8] ; y1
	mov bx, [bp+4] ; y2 
	; dx = y2 - y1
	mov dx, bx 
	sub dx, ax
	push di 
	push cx 
	push es
	call sinX10
	mov ax, [di] ; ax = sin(angle) * 10
	push cx      ; save cx 
	mov cx, dx 
	xor dx, dx  
	imul cx
	mov cx, ANGLE_DIV
	xor dx, dx 
	cwd
	idiv cx
	sub [si], ax ; x' -= sin(angle) * (y2 - y1)
	pop cx

; y' calculation, where y' is the y2 point coordinate after rotatin by a given angle
	; y' = cos(angle) * (y2 - y1) + sin(angle) * (x2 - x1) + y1

	; calculate cos(angle) * (y2 - y1):
	xor dx, dx
	mov ax, [bp+8]  ; y1
	mov bx, [bp+4]  ; y2
	mov [si+2], ax  ; y' = y1 
	; dx = y2 - y1
	mov dx, bx 
	sub dx, ax
	push di 
	push cx 
	push es
	call cosX10
	mov ax, [di] ; ax = cos(angle) * 10
	push cx      ; save cx 
	mov cx, dx  
	xor dx, dx 
	imul cx
	mov cx, ANGLE_DIV
	xor dx, dx 
	cwd
	idiv cx
	add [si+2], ax ; y' += cos(angle) * (y2 - y1)
	pop cx 
	
	; calculate sin(angle) * (x2 - x1):
	xor dx, dx
	mov ax, [bp+10]  ; x1
	mov bx, [bp+6]   ; x2
	; dx = x2 - x1
	mov dx, bx 
	sub dx, ax
	push di 
	push cx 
	push es
	call sinX10
	mov ax, [di] ; ax = sin(angle) * 10
	push cx ; save cx 
	mov cx, dx  
	xor dx, dx
	imul cx
	mov cx, ANGLE_DIV
	xor dx, dx 
	cwd
	idiv cx
	add [si+2], ax ; y' += sin(angle) * (x2 - x1)
	pop cx 

	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop di 
	pop si
	pop bp 
	ret 16
endp rotatePointAroundOtherPoint

proc rotateTriangleAboutItsCenter
	push bp 
	mov bp, sp 
	
	push si 
	push di
	push es
	push ax 
	push bx 
	push cx 
	push dx 
	
	mov es, [bp+22] ; the offset of 'sinesx10' variable in the DATASEG 
	mov di, [bp+18] ; the offset of 'result' variable in the DATASEG
	xor bx, bx 
	xor si, si 

; calculate the y value of the center of the triangle:
	xor ax, ax
	mov si, [bp+12] ; si = offset of y1
	mov ax, [si]    ; ax = value of y1 
 	mov si, [bp+8]  ; si = offset of y2
	add ax, [si]		; add value of y2 to ax
	mov si, [bp+4]  ; si = offset of y3
	add ax, [si]		; add value of y3 to ax

	xor dx, dx
	mov cx, 3
	;inc ax
	div cx
	xor dx, dx
	mov bx, ax ; bx now contains the y coordinate of the center of the triangle.

; calculate the x value of the center of the triangle:
	xor ax, ax 
	mov si, [bp+14] ; si = offset of x1
	mov ax, [si]		; ax = value of x1
	mov si, [bp+10] ; si = offset of x2
	add ax, [si]		; add value of x2 to ax
	mov si, [bp+6]  ; si = offset of x3
	add ax, [si]		; add value of x3 to ax

	xor dx, dx 
	mov cx, 3
	div cx 
	xor dx, dx 
; ax now contains the x coordinate of the center of the triangle
	
	; CenterPoint(ax, bx)

; rotate Point1(x1, y1) around CenterPoint(ax, bx)
	xor cx, cx 
	mov si, [bp+14] ; si = offset of x1
	mov cx, [si] 		; cx = value of x1
	mov si, [bp+12] ; si = offset of y1
	mov dx, [si]		; dx = value of y1
	mov si, [bp+20] ; si = offset of 'rotationResult' vairable in the DATASEG
	mov di, [bp+18] ; the offset of 'result' variable in the DATASEG

	push es
	push si 
	push di 
	mov si, [bp+16] ; si = angle
	push si 
	push ax
	push bx 
	push cx 
	push dx 
	call rotatePointAroundOtherPoint
	xor cx, cx
	mov si, [bp+20] ; si = offset of 'rotationResult' vairable in the DATASEG
	mov di, [bp+14] ; di = offset of x1 
	mov cx, [si]		; cx = value of x1' 
	mov [di], cx	  ; x1 is equals to x1' now	
	mov di, [bp+12] ; di = offset of y1 
	mov cx, [si+2]	; cx = value of y1'
	mov [di], cx		; y1 is equals to y1' now

; rotate Point2(x2, y2) around CenterPoint(ax, bx)	
	xor cx, cx 
	xor dx, dx
	mov si, [bp+10] ; si = offset of x2
	mov cx, [si] 		; cx = value of x2
	mov si, [bp+8]  ; si = offset of y2
	mov dx, [si]		; dx = value of y2
	mov si, [bp+20] ; si = offset of 'rotationResult' vairable in the DATASEG
	mov di, [bp+18] ; the offset of 'result' variable in the DATASEG
	
	push es
	push si 
	push di 
	mov si, [bp+16] ; si = angle
	push si 
	push ax
	push bx 
	push cx 
	push dx 
	call rotatePointAroundOtherPoint
	xor cx, cx
	xor dx, dx 
	mov si, [bp+20] ; si = offset of 'rotationResult' vairable in the DATASEG
	mov di, [bp+10] ; di = offset of x2
	mov cx, [si]		; cx = value of x2' 
	mov [di], cx	  ; x2 is equals to x2' now	
	mov di, [bp+8]  ; di = offset of y2 
	mov cx, [si+2]	; cx = value of y2'
	mov [di], cx		; y2 is equals to y2' now

; rotate Point3(x3, y3) around CenterPoint(ax, bx)	
	xor cx, cx
	mov si, [bp+6]  ; si = offset of x3
	mov cx, [si] 		; cx = value of x3
	mov si, [bp+4]  ; si = offset of y3
	mov dx, [si]		; dx = value of y3
	mov si, [bp+20] ; si = offset of 'rotationResult' vairable in the DATASEG
	mov di, [bp+18] ; the offset of 'result' variable in the DATASEG
	
	push es
	push si 
	push di 
	mov si, [bp+16] ; si = angle
	push si 
	push ax
	push bx 
	push cx 
	push dx 
	call rotatePointAroundOtherPoint
	xor cx, cx
	mov si, [bp+20] ; si = offset of 'rotationResult' vairable in the DATASEG
	mov di, [bp+6]  ; di = offset of x3
	mov cx, [si]		; cx = value of x3' 
	mov [di], cx	  ; x3 is equals to x3' now	
	mov di, [bp+4]  ; di = offset of y3 
	mov cx, [si+2]	; cx = value of y3'
	mov [di], cx		; y3 is equals to y3' now
	
	pop dx
	pop cx
	pop bx 
	pop ax 
	pop es 
	pop di 
	pop si
	pop bp 
	ret 20
endp rotateTriangleAboutItsCenter

proc drawStraightLine
	push bp 
	mov bp, sp 
	push bx ; start coordinates 
	push ax 
	push cx ; color
	push es ; graphic screen coordinates 
	push si ; direction 
	push dx 
	push di 
	
	mov es, [bp+8]
	mov si, [bp+6] ; the offset of the 'result' variable in the DATASEG
	mov bx, [bp+4]
	
	xor dx, dx 
	mov ax, bx 
	dec ax 
	mov di, SCREEN_WIDTH 
	div di 
	xor di, di 
	
	mov cl, RED_PIXEL ; red color 
	
	cmp si, UP_DIRECTION
	je drawUpStraightLineLoop
	cmp si, RIGHT_DIRECTION
	je drawRightStraightLineLoop
	cmp si, DOWN_DIRECTION
	je drawDownStraightLineLoop
	cmp si, LEFT_DIRECTION
	je drawLeftStraightLineLoop
	jmp endDrawStraightLine
	
	drawUpStraightLineLoop:
	call delay
	mov [es:bx], cl
	sub bx, 320 
	cmp bx, 320 
	jae drawUpStraightLineLoop
	jmp endDrawStraightLine
	
	drawRightStraightLineLoop:
	call delay
	xor dx, dx 
	mov [es:bx], cl 
	inc bx   
	mov ax, bx 
	mov di, 320 
	div di 
	cmp dx, 0 
	jne drawRightStraightLineLoop
	jmp endDrawStraightLine
	
	drawDownStraightLineLoop:
	call delay 
	mov [es:bx], cl 
	add bx, 320 
	cmp bx, 320*200 
	jbe drawDownStraightLineLoop
	jmp endDrawStraightLine
	
	drawLeftStraightLineLoop:
	call delay
	mov [es:bx], cl 
	sub bx, 1
	inc di 
	cmp di, dx
	jne drawLeftStraightLineLoop
	
endDrawStraightLine:
	pop di 
	pop dx 
	pop si 
	pop es
	pop cx 
	pop ax 
	pop bx 
	pop bp
	ret 6
endp drawStraightLine

proc drawFilledTriangleBottom
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx 
	push si
	push di
	
	mov es, [bp+24] ; offset of graphic screen
	mov di, [bp+22] ; offset of 'variables' array in the DATASEG 
	mov si, [bp+18] ; offset of 'drawFilledVariables' array in the DATASEG
	mov ax, [bp+14] ; value of x1
	mov bx, [bp+12] ; value of y1 
	mov cx, [bp+10] ; value of x2 
	mov dx, [bp+8]  ; value of y2 

;---------------------------------------------------
; calculate invslope1
	sub cx, ax 
	sub dx, bx
	push ax ; save the value of ax 
	push bx ; save the value of bx
	mov bx, dx
	push dx ; save the value of dx 
	xor dx, dx ; clear dx

	mov ax, cx 
	div bx
	xor dx, dx
	mov [si], ax

	pop dx  ; return the previous value of dx
	pop bx  ; return the previous value of bx
	pop ax  ; return the previous value of ax
;---------------------------------------------------

;---------------------------------------------------
; calculate invslope2

	mov cx, [bp+6] ; value of x3
	mov dx, [bp+4] ; value of y3
	sub cx, ax
	sub dx, bx
	push ax ; save the value of ax 
	push bx ; save the value of bx
	mov bx, dx 
	push dx ; save the value of dx 
	xor dx, dx ; clear dx

	mov ax, cx 
	div bx
	xor dx, dx
	mov [si+2], ax

	pop dx  ; return the previous value of dx
	pop bx  ; return the previous value of bx
	pop ax  ; return the previous value of ax
;---------------------------------------------------

;---------------------------------------------------
; draw lines that will be the triangle's bottom

	mov cx, ax
bottomFillScanLinesLoop:
	push [bp+16] ; the color of the triangle
	push es
	push ax
	push bx
	push cx
	push bx
	push [bp+20] ; offset of 'result' variable in the DATASEG
	push [bp+22] ; offset of 'vairables' array in the DATASEG
	call drawLine

	add ax, [si]
	add cx, [si+2]
	inc bx

	cmp bx, [bp+8] ; compare the current y to y2
	jae bottomFillScanLinesLoop
;---------------------------------------------------

	pop di
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	pop es
	pop bp
	ret 22
endp drawFilledTriangleBottom

proc drawFilledTriangleTop
	push bp 
	mov bp, sp
	push es 
	push ax 
	push bx
	push cx 
	push dx 
	push si 
	push di 
	
	mov es, [bp+24] ; offset of graphic screen
	mov di, [bp+22] ; offset of 'variables' array in the DATASEG 
	mov si, [bp+18] ; offset of 'drawFilledVariables' array in the DATASEG
	mov ax, [bp+6] ; value of x3
	mov bx, [bp+4] ; value of y3
	mov cx, [bp+12] ; value of x1
	mov dx, [bp+10] ; value of y1

;---------------------------------------------------
; calculate invslope1
	sub cx, ax 
	sub dx, bx
	push ax ; save the value of ax 
	push bx ; save the value of bx
	mov bx, dx 
	push dx ; save the value of dx 
	xor dx, dx ; clear dx

	mov ax, cx 
	div bx
	xor dx, dx
	mov [si], ax

	pop dx  ; return the previous value of dx
	pop bx  ; return the previous value of bx
	pop ax  ; return the previous value of ax
;---------------------------------------------------

;---------------------------------------------------
; calculate invslope2

	mov cx, [bp+10] ; value of x2
	mov dx, [bp+8] ; value of y2
	sub cx, ax
	sub dx, bx
	push ax ; save the value of ax 
	push bx ; save the value of bx
	mov bx, dx 
	push dx ; save the value of dx 
	xor dx, dx ; clear dx

	mov ax, cx 
	div bx
	xor dx, dx
	mov [si+2], ax

	pop dx  ; return the previous value of dx
	pop bx  ; return the previous value of bx
	pop ax  ; return the previous value of ax
;---------------------------------------------------

;---------------------------------------------------
; draw lines that will be the triangle's bottom
	mov bx, [bp+4] ; value of y3
	mov cx, [bp+6] ; value of x3
topFillScanLinesLoop:
	push [bp+16] ; the color of the triangle
	push es
	push ax
	push bx 
	push bx
	push dx
	push [bp+20] ; offset of 'result' variable in the DATASEG
	push [bp+22] ; offset of 'vairables' array in the DATASEG
	call drawLine

	sub ax, [si]
	sub cx, [si+2]
	dec bx

	cmp bx, [bp+12] ; compare the current y to y1
	jbe topFillScanLinesLoop
;---------------------------------------------------

	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax 
	pop es
	pop bp 
	ret 22
endp drawFilledTriangleTop

; TODO
proc drawFilledTriangle
	push bp 
	mov bp, sp
	push es 
	push ax 
	push bx
	push cx 
	push dx 
	push si 
	push di 
	
	mov ax, [bp+14] ; the value of x1
	mov bx, [bp+12] ; the value of y1
	mov cx, [bp+10] ; the value of x2
	mov dx, [bp+8]	; the value of y2
	mov di, [bp+6]	; the value of x3
	mov si, [bp+4]	; the value of y3

;---------------------------------------------------
; sort points by y coordinate so y3 >= y2 >= point1.y1
	; check if y1 > y2 and sort if true
	cmp bx, dx 
	jbe skipSortY1Y2
	; sort (x1, y1) and (x2, y2)
	xchg ax, cx
	xchg bx, dx 
skipSortY1Y2:
	; check if y1 > y3 and sort if true
	cmp bx, si 
	jbe skipSortY1Y3
	; sort (x1, y1) and (x3, y3)
	xchg ax, di
	xchg bx, si
skipSortY1Y3:
	; check if y2 > y3 and sort if true
	cmp dx, si 
	jbe skipSortY2Y3
	; sort (x2, y2) and (x3, y3)
	xchg cx, di
	xchg dx, si
skipSortY2Y3:
;---------------------------------------------------

;---------------------------------------------------
; check if y2 == y3, if true draw only bottom
	cmp dx, si
	jne skipDrawFilledBottomOnly
	; draw only bottom
	push [bp+24] 
	push [bp+22]
	push [bp+20]
	push [bp+18]
	push [bp+16]
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	call drawFilledTriangleBottom
	jmp endDrawFilledTriangle

skipDrawFilledBottomOnly:

; check if y1 == y2, if true draw only top
	cmp bx, dx
	jne skipDrawFilledTopOnly
	; draw top only 
	push [bp+24] 
	push [bp+22]
	push [bp+20]
	push [bp+18]
	push [bp+16]
	push ax
	push bx
	push cx
	push dx
	push di
	push si
	call drawFilledTriangleTop
	jmp endDrawFilledTriangle

skipDrawFilledTopOnly:
;---------------------------------------------------

endDrawFilledTriangle:
	pop di
	pop si 
	pop dx 
	pop cx 
	pop bx
	pop ax 
	pop es 
	pop bp
	ret 22
endp drawFilledTriangle

proc drawAllTriangles
	; Function that draws all triangles according to given arrays with data that include the colors, x and y co-ordiantes.
	push bp 
	mov bp, sp 
	push ax 
	push bx 
	push cx 
	push di
	push si
	push es

	xor cx, cx 	
	
	mov es, [bp+12] ; the offset of the graphical screen 
	mov si, [bp+10] ; the offset of 'trianglesXCoordinates' array in the DATASEG
	mov di, [bp+8] ; the offset of 'trianglesYCoordinates' array in the DATASEG

drawAllTrianglesLoop:

	mov bx, [bp+14] ; the offset of 'trianglesColors' or 'trianglesClearColors' array in the DATASEG
	xor ax, ax  
	add bx, cx 
	mov al, [byte ptr bx]
	push ax 
	push es 

pushPointCoordinatesLoop:
	mov ax, [word ptr si] ; save x value in ax 
	cmp ax, VALUES_SEPARATOR
	je skipPushPointCoordinatesLoop
	push ax
	add si, 2 ; mov x values pointer to point on the next x

	mov ax, [word ptr di] ; save y value in ax 
	push ax 
	add di, 2 ; mov y values pointer to point on the next y
	
	jmp pushPointCoordinatesLoop
skipPushPointCoordinatesLoop:
	add si, 2 ; skip separator value 
	add di, 2 ; skip separator value
  
	xor bx, bx
	mov bx, [bp+6] ; offset of 'result' variable in the DATASEG
	push bx 
	mov bx, [bp+4] ; offset of 'variables' array in the DATASEG
	push bx 
	
	call drawTriangle

inc cx
cmp cx, [bp+16] ;  the value of 'trianglesAmount' variable in the DATASEG
je skipDrawAllTrianglesLoop
jmp drawAllTrianglesLoop

skipDrawAllTrianglesLoop:

	pop es 
	pop si 
	pop di 
	pop cx 
	pop bx
	pop ax 
	pop bp 
	ret 14
endp drawAllTriangles

proc addTriangle
	push bp 
	mov bp, sp 
	push ax 
	push bx 
	push cx 
	push dx 
	push si 
	push di 
	
	mov bx, [bp+24] ; the offset of 'trianglesAmount' variable in the DATASEG
	mov cx, [bx]    ; move the value of trianglesAmount into cx, instead of the offset
	shl cx, 3 		  ; calculate the highset of the next point values 
	mov di, [bp+22] ; the offset of 'trianglesXCoordinates' array in the DATASEG
	mov si, [bp+20] ; the offset of 'trianglesYCoordinates' array in the DATASEG
	add di, cx 		  ; calculate x co-ordinates highset
	add si, cx 		  ; calculate y co-ordinates highset
	shr cx, 3 		  ; return cx to its original value 
	inc cx 
	mov bx, [bp+24] ; the offset of 'trianglesAmount' variable in the DATASEG
	mov [bx], cx    ; update the amount of triangles 
	
	;mov cx, 3 ; cx = the amount of points the triangle have 

; setUpXAndYCoordinatesLoop:
; 	xor ax, ax 
; 	mov ax, [bp+] ; the value of x1
; 	mov [di], ax
; 	add di, 2
; 	mov ax, [bp+] ; the value of y1
; 	mov [si], ax
; 	add si, 2
; loop setUpXAndYCoordinatesLoop

	xor ax, ax 
	mov ax, [bp+16] ; the value of x1
	mov [di], ax
	add di, 2
	mov ax, [bp+14] ; the value of y1
	mov [si], ax
	add si, 2
	
	mov ax, [bp+12] ; the value of x2
	mov [di], ax
	add di, 2
	mov ax, [bp+10] ; the value of y2
	mov [si], ax
	add si, 2
	
	mov ax, [bp+8] ; the value of x3
	mov [di], ax
	add di, 2
	mov ax, [bp+6] ; the value of y3
	mov [si], ax
	add si, 2

; set values separators at the end of the x and y co-ordinates setup loop
	mov bx, VALUES_SEPARATOR
	mov [di], bx
	mov [si], bx

	mov bx, [bp+18] ; the offset of 'trianglesColors' array in the DATASEG
	dec cx 
	add bx, cx
	mov ax, [bp+4] ; the value of the color for the triangle 
	mov [byte ptr bx], al

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 22
endp addTriangle 

proc rotateAllTriangles
	push bp 
	mov bp, sp 
	push ax
	push bx
	push cx
	push dx 
	push si 
	push di
	
	xor bx, bx
	xor ax, ax
	xor cx, cx ; clear cx 
	mov si, [bp+6] ; the offset of 'trianglesXCoordinates' array in the DATASEG
	mov di, [bp+4] ; the offset of 'trianglesYCoordinates' array in the DATASEG

rotateAllTrianglesLoop:
	mov bx, [bp+16] ; the offset of 'sinesX10' array in the DATASEG 
	push bx 
	mov bx, [bp+14] ; the offset of 'rotationResult' array in the DATASEG 
	push bx 
	mov bx, [bp+12] ; the offset of 'result' variable in the DATASEG
	push bx 
	mov bx, [bp+8] ; the offset of 'trianglesDegreeRotationValues' array in the DATASEG
	shl cx, 1 ; adjust types 
	add bx, cx
	shr cx, 1 ; return original values 
	push [bx] ; push the degree value inside the offset 

pushPointsOffsetsLoop: 
	mov ax, [word ptr si] ; save the value of x in ax 
	cmp ax, VALUES_SEPARATOR
	je skipPushPointsOffsets
	push si
	add si, 2 ; mov x offsets pointer to point on the next x

	push di 
	add di, 2 ; mov y offsets pointer to point on the next y
	jmp pushPointsOffsetsLoop

skipPushPointsOffsets:
; skip vlaues separators offsets 
	add si, 2 
	add di, 2
	
	call rotateTriangleAboutItsCenter

inc cx
cmp cx, [bp+10] ; the value of 'trianglesAmount' variable in the DATASEG 
je skipRotateAllTrianglesLoop
jmp rotateAllTrianglesLoop

skipRotateAllTrianglesLoop:

	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax	
	pop bp
	ret 14
endp rotateAllTriangles

proc addTriangleViaInput
	push bp 
	mov bp, sp 
	push ax 
	push bx 
	push cx 
	push dx 
	push si 
	push di 	
	push es 

	xor ax, ax 
	xor bx, bx 
	xor cx, cx
	xor dx, dx
	
	mov si, [bp+18] ; the offset of 'pointsXCoordinatesInputedBuffer' array in the DATASEG
	mov di, [bp+16] ; the offset of 'pointsYCoordinatesInputedBuffer' array in the DATASEG 
	mov bx, [bp+14] ; the offset of 'pointsInputedAmount' variable in the DATASEG
	; add current offset 
	mov ax, [bx]
	shl ax, 1 
	add si, ax 
	add di, ax 

	; set the clock offset to es
	mov ax, 40h
	mov es, ax 
	mov cx, 16 ; ticks amount to wait, ~0.88 seconds
	
	mov ax, [CLOCK]

firstTick:
	cmp ax, [CLOCK]
	je firstTick 

trianglePointsInputLoop:
	push cx 
	mov ax, [CLOCK]
Tick:
	cmp ax, [CLOCK]
	je Tick 

	xor ax, ax
	; hide mouse cursor
	mov ax, 2h
	int 33h 
	xor bx, bx
	xor cx, cx 
	xor dx, dx 
	mov ax, 3h 
	int 33h 
	cmp bx, 01h 
	jne skipAddPointToInputed
	mov bx, [bp+14]
	mov ax, 3
	; check if the user have inputed enough points 
	cmp [bx],  ax
	je skipAddPointToInputed

	shr cx, 1   ; adjust cx value to the proper x co-ordinate 
	mov [si], cx 
	mov [di], dx 
	add si, 2  ; update the pointer of x co-ordinates to point on the next x co-ordinates offset
	add di, 2  ; update the pointer of y co-ordinates to point on the next y co-ordinates offset
	mov bx, [bp+14] ; the offset of 'pointsInputedAmount' variable in the DATASEG

	; update the amount of points inputed counter value
	mov cx, [bx]
	inc cx 
	mov [bx], cx 

skipAddPointToInputed:
	; put mouse cursor back on screen 
	mov ax, 1h
	int 33h 

	pop cx 
loop trianglePointsInputLoop

	mov bx, [bp+14]
	mov ax, 3
	cmp [bx], ax
	jne skipCallAddTriangleFunction

	push [bp+10] ; the offset of 'trianglesAmount' variable in the DATASEG
	push [bp+8]  ; the offset of 'trianglesXCoordinates' array in the DATASEG
	push [bp+6]  ; the offset of 'trianglesYCoordinates' array in the DATASEG
	push [bp+4]  ; the offset of 'trianglesColors' array in the DATASEG
	mov cx, 3
	; return the originals offsets to si and di 
	mov si, [bp+18] ; the offset of 'pointsXCoordinatesInputedBuffer' array in the DATASEG
	mov di, [bp+16] ; the offset of 'pointsYCoordinatesInputedBuffer' array in the DATASEG
coordinatesPushLoop:
	push [si]
	add si, 2
	push [di]
	add di, 2
loop coordinatesPushLoop
	push [bp+12] ; the value of 'currentColor' variable in the DATASEG
	call addTriangle

	xor ax, ax
	mov [bx], ax  	; initialize the value of 'pointsInputedAmount' variable
	
	mov bx, [bp+20] ; the offset of 'isTPressed' variable in the DATASEG
	mov [bx], ax    ; set 0 to the value of 'isTPressed' so it will determine that 't' has not been pressed

skipCallAddTriangleFunction:
	pop es 
	pop di 
	pop si 
	pop dx 
	pop cx 
	pop bx 
	pop ax
	pop bp 
	ret 18
endp addTriangleViaInput



start:
	mov ax, @data
	mov ds, ax 
	mov ax, 0A000h 
	mov es, ax 
	xor ax, ax 
	
	mov ax, 13h 
	int 10h
	
	push es 
	call clearGraphicScreen
	
	mov ax, 0000h ; initialize the mouse 
	int 33h 
	
	mov ax, 1h ; put mouse 
	int 33h 

	; push offset trianglesAmount        ; + 24
	; push offset trianglesXCoordinates  ; + 22
	; push offset trianglesYCoordinates	 ; + 20
	; push offset trianglesColors        ; + 18
	; push 175	; x1										 ; + 16
	; push 100	; y1                     ; + 14
	; push 225	; x2										 ; + 12
	; push 125	; y2                     ; + 10
	; push 200	; x3										 ; + 8
	; push 175	; y3										 ; + 6
	; push 15	  ; color									 ; + 4
	; call addTriangle	
	
	; push offset trianglesAmount        ; + 24
	; push offset trianglesXCoordinates  ; + 22
	; push offset trianglesYCoordinates	 ; + 20
	; push offset trianglesColors        ; + 18
	; push 50   ; x1										 ; + 16
	; push 50   ; y1                     ; + 14
	; push 100  ; x2										 ; + 12
	; push 50   ; y2                     ; + 10
	; push 50  	; x3										 ; + 8
	; push 100	; y3										 ; + 6
	; push 13	  ; color									 ; + 4
	; call addTriangle

spinLoop:

	push [trianglesAmount]            ; + 16
	push offset trianglesColors				; + 14
	push GRAPHICAL_SCREEN_VALUE       ; + 12
	push offset trianglesXCoordinates ; + 10
	push offset trianglesYCoordinates ; + 8
	push offset result    						; + 6
	push offset variables 						; + 4
	call drawAllTriangles
	
	push offset isTPressed                      ; + 20
	push offset pointsXCoordinatesInputedBuffer ; + 18
	push offset pointsYCoordinatesInputedBuffer ; + 16
	push offset pointsInputedAmount             ; + 14
	xor ax, ax 
	mov al, [currentColor]
	push ax 																		; + 12
	push offset trianglesAmount                 ; + 10
	push offset trianglesXCoordinates           ; + 8
	push offset trianglesYCoordinates           ; + 6
	push offset trianglesColors                 ; + 4
	call addTriangleViaInput
	
	push [trianglesAmount]            ; + 16
	push offset trianglesClearColors	; + 14
	push GRAPHICAL_SCREEN_VALUE       ; + 12
	push offset trianglesXCoordinates ; + 10
	push offset trianglesYCoordinates ; + 8
	push offset result    						; + 6
	push offset variables 						; + 4
	call drawAllTriangles
	
	push offset sinesX10											; + 16
	push offset rotationResult 								; + 14
	push offset result                        ; + 12
	push [trianglesAmount]									  ; + 10
	push offset trianglesDegreeRotationValues ; + 8
	push offset trianglesXCoordinates					; + 6
	push offset trianglesYCoordinates					; + 4
	call rotateAllTriangles
	jmp spinLoop

MainLoop:
	xor bx, bx
	xor ax, ax
	mov ax, 1h ; put mouse 
	int 33h 
	mov ah, 01h ; direction input 
	int 16h 
	jz noinput
	mov ah, 00h 
	int 16h 
	cmp al, 'l' 
	je LineDraw
	;cmp al, 't'
	;je TriangleDraw
	cmp al, ESCAPE_KEY
	je preExit 
	;cmp al, 'w'
	;je UpDir
	;cmp al, 's' 
	;je DownDir
	;cmp al, 'a' 
	;je LeftDir
	;cmp al, 'd' 
	;je RightDir 
	
noinput:
	
	xor bx, bx 
	xor cx, cx ; x coordinate of a point 
	xor dx, dx ; y coordinate of a point 
	mov ax, 3h ; get input from mouse and its coordinates 
	int 33h 
	
	shr cx, 1 
	mov [pointx], cx
	mov [pointy], dx 
	
	cmp bx, 01h ; check if left button pressed 
	jne MainLoop
	
	; hide mouse cursor 
	mov ax, 2h
	int 33h 
	
	; line start coordinates 
	mov bx, offset result
	; dx now is the x and cx is the y: 
	mov dx, [pointx]
	mov cx, [pointy] 
	
	push bx 
	push dx 
	push cx 
	call pointCoordinatesToPixelCoordinate
	mov ax, [result]
	
	; draw line 
	push es 
	push si 
	push ax 
	call drawStraightLine 
	
jmp MainLoop

preExit: jmp exit

LineDraw:
Point1:
	xor bx, bx 
	xor cx, cx 
	xor dx, dx 
	xor ax, ax 
	mov ax, 3h ; get input from mouse 
	int 33h
	cmp bx, 0000000000000001b ; is left button pressed ?
	jne Point1
	shr cx, 1
	mov [point1x], cx 
	mov [point1y], dx

	call delay2

Point2: 
	xor bx, bx
	xor cx, cx 
	xor dx, dx 
	xor ax, ax
	mov ax, 3h ; get input from mouse  
	int 33h 
	cmp bx, 0000000000000001b ; is left button pressed ?
	jne Point2
	shr cx, 1
	mov [point2x], cx 
	mov [point2y], dx

; calls the drawLine function:
	push GREEN_PIXEL
	push es 
	mov ax, [point1x]
	push ax
	mov bx, [point1y]
	push bx 
	mov cx, [point2x]
	push cx 
	mov dx, [point2y]
	push dx
	mov si, offset result
	push si 
	mov di, offset variables
	push di 
	call drawLine
	jmp MainLoop

UpDir:
	mov si, UP_DIRECTION
	jmp noinput
DownDir:
	mov si, DOWN_DIRECTION
	jmp noinput
LeftDir:
	mov si, LEFT_DIRECTION
	jmp noinput
RightDir:
	mov si, RIGHT_DIRECTION
	jmp noinput
	
exit:
	; return to textual screen 
	xor ah, ah 
	mov al, 2
	int 10h

	mov ax, 4c00h 
	int 21h 
END start

; TODO

; 	**IMP. !!! ADD DOCUMENTATION FOR ALL FUNCTIONS !!! 
;	    2. add start screen to the code.
;		7i. function that does a delay by given as parameter amount of time.
;     s. create cool and relaxing music and add it - MUSIC CREATED, 
; Imp1. Create and add MULTITHREADING 
