[org 0x0100]

jmp start

startmessage: db 'Welcome to Sudoko',0
easy: db 'Easy',0
medium: db 'Medium',0
hard: db 'Hard',0
instructionmessage: db 'Press 1 for Easy, 2 for Medium and 3 for Hard',0
chooselevelsmessage: db 'Choose the level complexity ',0
totalscore: db 'Total Score : 0',0			;For static view score by default is zero
timetaken: db 'Time taken :  :  ',0
endmessage: db 'Thanks for playing Sudoku!', 0
score_message: db 'Score:0 ',0
mistakes_message: db 'Mistakes: /5',0
time_message: db 'Time:   :',0
print_numbers: db '1   2   3   4   5   6   7   8   9',0
lostmessage: db 'You Lost mistakes 5/5',0
instructions: db 'INSTRUCTIONS',0
movement_message: db 'Press s to go downward, w to go upward, a to go left, d to go right',0
pagenumber: db 'Press c for the first page and v for the second page',0
endscreen: db 'Press escape key to move to end the game',0
undorule: db 'Press u for undo',0
eraserule: db 'Press 0 for erase',0
notesmessage: db 'Press n to enable and disable notes',0
mistake: dw 0
second: dw 0   
minute: dw 0               
tickcount:    dw   0
oldisr:       dd   0
currentLevel: db 0 
current: dw 0 
currentX: dw 16
currentY: dw 6
currentPage: dw 0xb900 
previousPage: dw 0
temp: dw 0
notes: dw 0 
pencil: db 'Pencil',0
currentGridColor: db 0
noOFcards:   db   '000000000',0
easyCards:   db   '243346466',0
mediumCards: db   '414336526',0
hardCards:   db   '343236355',0
score: dw 0 
undoArr: times 180  dw   0 
currUndo: dw  0 
; Easy Board 
gridborder:     db '+-----------------+-----------------+-----------------+',0
easy_gridrow1:  db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow2:  db '|  6  |  8  |     |     |  5  |  2  |  7  |  1  |  4  |',0
easy_gridrow3:  db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow4:  db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow5:  db '|  7  |  5  |  2  |  6  |  4  |     |  8  |     |  9  |',0
easy_gridrow6:  db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow7:  db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow8:  db '|  1  |  3  |     |     |     |  8  |  5  |  6  |     |',0
easy_gridrow9:  db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow10: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow11: db '|  4  |  9  |  8  |  5  |  6  |     |     |     |     |',0
easy_gridrow12: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow13: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow14: db '|     |     |     |     |     |     |  9  |     |  6  |',0
easy_gridrow15: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow16: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow17: db '|  2  |  6  |  7  |  9  |  1  |  3  |     |     |  8  |',0
easy_gridrow18: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow19: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow20: db '|  5  |  2  |     |  8  |     |  9  |  6  |     |     |',0
easy_gridrow21: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow22: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow23: db '|     |  7  |  6  |     |  2  |     |  3  |  9  |     |',0
easy_gridrow24: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow25: db '|     |     |     |     |     |     |     |     |     |',0
easy_gridrow26: db '|  9  |  4  |  3  |     |  7  |  6  |     |  8  |  5  |',0
easy_gridrow27: db '|     |     |     |     |     |     |     |     |     |',0




; Medium Board
medium_gridrow1:  db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow2:  db '|  9  |  4  |  1  |  8  |     |  6  |     |     |  7  |', 0
medium_gridrow3:  db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow4:  db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow5:  db '|     |     |  6  |     |  3  |  7  |     |  1  |  5  |', 0
medium_gridrow6:  db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow7:  db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow8:  db '|  7  |     |  3  |     |  9  |  1  |     |  4  |     |', 0
medium_gridrow9:  db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow10: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow11: db '|  1  |  8  |  2  |  5  |     |     |     |     |  6  |', 0
medium_gridrow12: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow13: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow14: db '|  6  |     |     |     |  7  |     |  4  |     |  9  |', 0
medium_gridrow15: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow16: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow17: db '|  4  |  7  |  9  |  6  |     |     |     |     |     |', 0
medium_gridrow18: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow19: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow20: db '|  3  |     |     |     |     |     |  7  |     |     |', 0
medium_gridrow21: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow22: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow23: db '|     |  6  |     |     |     |  8  |     |  9  |  3  |', 0
medium_gridrow24: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow25: db '|     |     |     |     |     |     |     |     |     |', 0
medium_gridrow26: db '|  5  |  9  |     |     |     |     |     |  6  |  1  |', 0
medium_gridrow27: db '|     |     |     |     |     |     |     |     |     |', 0


; Hard Board
hard_gridrow1:  db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow2:  db '|  1  |  7  |  4  |  6  |  9  |     |  2  |     |     |', 0
hard_gridrow3:  db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow4:  db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow5:  db '|  2  |  5  |  8  |  7  |     |     |  9  |  6  |  3  |', 0
hard_gridrow6:  db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow7:  db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow8:  db '|     |     |  6  |  8  |     |  5  |     |     |     |', 0
hard_gridrow9:  db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow10: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow11: db '|  3  |     |  7  |  9  |  8  |     |  6  |     |     |', 0
hard_gridrow12: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow13: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow14: db '|  8  |  4  |  9  |  2  |  5  |  6  |     |     |     |', 0
hard_gridrow15: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow16: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow17: db '|     |  6  |     |     |     |     |     |     |  2  |', 0
hard_gridrow18: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow19: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow20: db '|  7  |     |     |  1  |     |     |     |  2  |     |', 0
hard_gridrow21: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow22: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow23: db '|     |     |     |     |     |  8  |     |     |     |', 0
hard_gridrow24: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow25: db '|     |     |     |     |     |     |     |     |     |', 0
hard_gridrow26: db '|     |     |  3  |     |     |     |  4  |  8  |  1  |', 0
hard_gridrow27: db '|     |     |     |     |     |     |     |     |     |', 0

button_press_sound:
	push ax 
	push bx 
	push cx
	
    mov al, 182              ; Command byte for PIT
    out 43h, al
    mov ax, 1193             ; Frequency divisor for ~400 Hz
    out 42h, al
    mov al, ah
    out 42h, al
    in al, 61h               ; Enable PC speaker
    or al, 00000011b
    out 61h, al
                    ; Short duration for button sound
    call short_delay

    ; Stop the sound quickly
    in al, 61h
    and al, 11111100b        ; Disable speaker
    out 61h, al

	pop cx 
	pop bx 
	pop ax 
    ret

short_delay:
    push cx
    ; Short delay loop
    mov cx, 50000             ; Very short delay
short_loop:
    dec cx
    jne short_loop
    pop cx
    ret

printBases:
; Assuming left, right is in bytes, not exact coords
; [bp+4] = attribute, 
; [bp+6] = right, [bp+8] = bottom, [bp+10] = left, [bp+12] = top
	push bp
	mov bp, sp
	push ax
	push cx
	push di
	push dx

	; Print top line
	mov ax, 80
	mul byte [bp+12]
	add ax, [bp+10]
	shl ax, 1
	mov di, ax  ; Location Acquired ((top*80 + left) * 2)
	mov cx, [bp+6]
	sub cx, [bp+10]  ; right - left = length of horizontal line
	sub cx, 2 ; top left and right characters not in loop
	mov ah, [bp+4]  ; Attribute
	mov al, 0xda ; top left char
	stosw
	mov al, 0xc4 ; horizontal char
	rep stosw
	mov al, 0xbf ; top right char
	stosw
	; Print vertical lines
	mov cx, [bp+8]
	sub cx, [bp+12]  ; bottom - top = length of vertical lines
	dec cx ; prevent overriding corner chars
	 
	mov dx, 80
	nextrow:
		; print left line
		mov ax, [bp+8]
		sub ax, cx  ; bottom - cx = current row
		mul dl
		add ax, [bp+10]
		shl ax, 1
		mov di, ax  ; Location Acquired (((bottom-cx)*80 + left) * 2)
		mov ah, [bp+4]  ; Attribute
		mov al, 0xb3
		mov [es:di], ax  
		; print right line
		mov ax, [bp+8]
		sub ax, cx  ; bottom - cx = current row
		mul dl
		add ax, [bp+6]
		dec ax  ; offset correction for right line
		shl ax, 1
		mov di, ax  ; Location Acquired (((bottom-cx)*80 + right) * 2)
		mov ah, [bp+4]  ; Attribute
		mov al, 0xb3
		mov [es:di], ax 
		loop nextrow
	; Print bottom line
	mov ax, 80
	mul byte [bp+8]
	add ax, [bp+10]
	shl ax, 1
	mov di, ax  ; Location Acquired ((bottom*80 + left) * 2)
	mov cx, [bp+6]
	sub cx, [bp+10]  ; right - left = length of horizontal line
	sub cx, 2 ; bottom left and right char not in loop
	mov ah, [bp+4]  ; Attribute
	mov al, 0xc0 ; bottom left char
	stosw
	mov al, 0xc4 ; horizontal char
	rep stosw
	mov al, 0xd9 ; bottom right char
	stosw
	

	pop dx
	pop di
	pop cx
	pop ax
	mov sp, bp
	pop bp
ret 10 ; 5 params = 10 bytes
	



printCards:
	pusha
	
	push 0xba00
	pop es
	; Print cards on pg2
	mov di, (80 * 22 + 11) * 2    ;22-> 15
	mov cx, 1
	mov bx, 9
	mov dx, 14
	loop1:
		push word 21 ;top       ; 21->14 
		push bx ;left
		push word 24 ;bottom    ;  24->17
		push dx ;right
		push word 3;  79h ;attribute
		call printBases
		mov al, cl
		add al, 30h
		mov ah, byte[currentGridColor]           ;  70h
		mov word [es:di], ax
		mov si, noOFcards
		add si, cx
		mov al, [si-1]
		mov ah, 3 ;7bh
		mov word [es:di + 160], ax
		
		; next card pos
		add bx, 7
		add dx, 7
		add di, 14
		inc cx
		cmp cx, 10
		jne loop1
	popa
ret


performUndo:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es
    push si
    push di

    ; Check if undo stack is empty
    mov bx, word[currUndo]
    cmp bx, 0
    je retPerformUndo    ; Exit if no undo is possible

    ; Retrieve last operation
    sub bx, 2
    mov ax, word[undoArr + bx]  
    mov es, ax
    sub bx, 2
    mov di, word[undoArr + bx]    ; Get memory address
    sub bx, 2
    mov ax, word[undoArr + bx]    ; Get value to restore
    mov word[currUndo], bx        ; Update undo pointer

    ; Restore the value
    and ah, 00001111b 
    mov word[es:di], ax

retPerformUndo:
    pop di
    pop si
    pop es
    pop dx
    pop ax
    pop bx
    pop cx
    pop bp
    ret
 


; subroutine to print a number at top left of screen
; takes the number to be printed as its parameter
printnum:
push bp
mov  bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov  ax, [bp+8]
mov  es, ax
mov  ax, [bp+4]
mov  bx, 10
mov cx, 0
nextdigit:
mov  dx, 0
div  bx
add  dl, 0x30
push dx
inc  cx
cmp  ax, 0
jnz  nextdigit
mov di, [bp+6]
nextpos:
pop  dx
mov  dh, 3
mov  [es:di], dx
add  di, 2
loop nextpos
pop  di
pop  dx
pop  cx
pop  bx
pop  ax
pop  es
pop  bp
ret  6
; timer interrupt service routine
timer:

push ax
push cx 
inc  word [cs:tickcount]; increment tick count
cmp  word [cs:tickcount] , 18 
jne return  
mov word [cs:tickcount] , 0
inc word [cs:second]

cmp word [cs:second], 60
jne leavewithoutupdateMinute
mov word [cs:second], 0
inc word [cs:minute]
mov cx , 0xb900
push cx 
mov cx, 446
push cx
push word [cs:minute]  
call printnum
leavewithoutupdateMinute:
mov cx , 0xb900
mov es , cx 
mov word[es:454] , 0x0720 
push cx
mov cx, 452
push cx
push word [cs:second]  
call printnum

return:
mov  al, 0x20
out  0x20, al
pop cx
pop ax 
iret

EnableTimer:
push ax 
push es 
xor  ax, ax
mov  es, ax
mov ax, [es:8*4]
mov [oldisr], ax
mov ax, [es:8*4+2]
mov [oldisr+2], ax
cli       ; disable interrupts
mov  word [es:8*4], timer ; store offset at n*4
mov  [es:8*4+2], cs
sti

pop ax 
pop es 
ret 


DisableTimer:
push es 
push ax 
push bx 
xor  ax, ax
mov  es, ax
mov ax, [oldisr]
mov bx, [oldisr+2]
cli
mov [es:8*4], ax
mov [es:8*4+2], bx
sti
pop bx 
pop ax 
pop es 
ret
clearCurrent:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 
    push di  

   mov ax , [previousPage] 
   mov es , ax
   mov di,[current]
   xor bx , bx 
box4:
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7) 
    mov word[es:di] , cx 

    add di , 2 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 
    add di , 2 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 

    sub di , 6 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 
     sub di , 2 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 
    add di , 4
    inc bx 
    cmp bx , 3 
    jae remain4
    cmp bx , 2 
    jae sts
    add di , 160
    jmp box4 
sts: 
    sub di , 320 
    jmp box4 

remain4:
    pop di
    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret 
	
	
calculateDi:
	push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 
	
	mov ax,80 
    mov bx, [bp+4]          
	mul bx        
	add ax, [bp+6]   
	shl ax,1              
	mov di,ax 
	
    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret 4
prominentBorder:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 
    push di  
    mov ax , 0xb900
	mov es , ax 
	mov ax,31
	push ax
	mov ax,5
	push ax
	call calculateDi
	xor cx , cx 
	
borderLoop: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 20
    jbe borderLoop
	
	mov ax,13
	push ax
	mov ax,5
	push ax
	call calculateDi
	xor cx,cx
	
borderLoop1: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 20
    jbe borderLoop1

	mov ax,49
	push ax
	mov ax,5
	push ax
	call calculateDi
	xor cx,cx
	
borderLoop2: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 20
    jbe borderLoop2
	
	mov ax,67
	push ax
	mov ax,5
	push ax
	call calculateDi
	xor cx,cx
	
borderLoop3: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 20
    jbe borderLoop3
	
	mov ax , 0xba00
	mov es , ax 
	mov ax,31
	push ax
	mov ax,0
	push ax
	call calculateDi
	xor cx , cx 
	
borderLoop4: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 16
    jbe borderLoop4
	
	mov ax,13
	push ax
	mov ax,0
	push ax
	call calculateDi
	xor cx,cx
	
borderLoop5: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 16
    jbe borderLoop5

	mov ax,49
	push ax
	mov ax,0
	push ax
	call calculateDi
	xor cx,cx
	
borderLoop6: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 20
    jbe borderLoop6
	
	mov ax,67
	push ax
	mov ax,0
	push ax
	call calculateDi
	xor cx,cx
	
borderLoop7: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 160
	inc cx 
	cmp cx , 16
    jbe borderLoop7
	
	mov ax , 0xb900
	mov es , ax 
	mov ax,14
	push ax
	mov ax,16
	push ax
	call calculateDi
	xor cx , cx 
	
rowborderLoop: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 2
	inc cx 
	cmp cx , 55
    jbe rowborderLoop
	
	mov ax , 0xba00
	mov es , ax 
	mov ax,14
	push ax
	mov ax,4
	push ax
	call calculateDi
	xor cx , cx 
	
rowborderLoop1: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 2
	inc cx 
	cmp cx , 55
    jbe rowborderLoop1

	mov ax,14
	push ax
	mov ax,16
	push ax
	call calculateDi
	xor cx,cx
	
rowborderLoop2: 
    mov bx , word[es:di]
    mov  bh, 0x05   
    mov word[es:di] , bx 
	add di , 2
	inc cx 
	cmp cx , 55
    jbe rowborderLoop2
    pop di
    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret  

	
 


setCurrent:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 
    push di  
	
	
    
    call clearCurrent

    mov ax, word[currentPage]
    mov word[previousPage] , ax
    mov ax,  [bp+4]
    mov word[currentPage] , ax ; Set current page
    mov ax,80 
    mov bx, 6          
	mul bx        
	add ax, 16        
	shl ax,1              
	mov di,ax 
    mov word[current] , di
    mov ax , [bp+4] 
    mov es , ax
    push di 
    push bx 
    xor bx , bx 
box:
    mov cx , word[es:di]
    or ch, 11110000b   ; Set the blinking bit and white background 
    mov word[es:di] , cx 

    add di , 2 
    mov cx , word[es:di]
    or ch, 11110000b   
    mov word[es:di] , cx 
    add di , 2 
    mov cx , word[es:di]
    or ch, 11110000b   
    mov word[es:di] , cx 

    sub di , 6 
    mov cx , word[es:di]
    or ch, 11110000b  
    mov word[es:di] , cx 
     sub di , 2 
    mov cx , word[es:di]
    or ch, 11110000b   
    mov word[es:di] , cx 
    add di , 4
    inc bx 
    cmp bx , 3 
    jae remain
    cmp bx , 2 
    jae st 
    add di , 160
    jmp box 
st: 
    sub di , 320 
    jmp box    

remain:
    pop bx 
    pop di 



    pop di
    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret 2 


isValidValuve:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    sub sp, 2 
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push di 
    mov ah , byte[currentGridColor]

    mov si , 0  
    mov es , word[currentPage]
    mov word[temp] , es
    mov bx , word[currentX]
    mov cx , word[currentY]
    mov [bp - 2] , bx 
    jmp moveLeft
retIs1:
    jmp retIs

moveLeft:
    cmp bx , 64 
    jae restoreBxToMoveRight
    add bx , 6
    push bx 
    push cx 
    call calculateDi
    mov dx , word[es:di] 
    cmp dx , ax 
    je retIs1
    jmp moveLeft
restoreBxToMoveRight:
    mov bx , [bp-2] 
moveRight:
    cmp bx , 16
    jbe restoreBxToMoveUpward
    sub bx , 6
    push bx 
    push cx 
    call calculateDi
    mov dx ,word[es:di]
    cmp dx , ax
    je retIs1
    jmp moveRight
moveUpwardsForPage2:
    cmp cx , 2
    jbe restoreBxToMoveUpwardForPage1
    sub cx , 4
    push bx 
    push cx 
    call calculateDi
    mov dx ,word[es:di]
    cmp dx , ax
    je retIs1
    jmp moveUpwardsForPage2
restoreBxToMoveUpwardForPage1:
    mov bx , [bp-2]
    mov cx , 26
    push ax 
    mov ax , 0xb900
    mov word[temp] , ax
    mov es , ax
    pop ax 
    jmp moveUpwards

restoreBxToMoveUpward:
    mov bx , [bp-2]
moveUpwards:
    cmp word[temp] , 0xba00
    je moveUpwardsForPage2
    cmp cx , 6
    jbe restoreBxToMoveDownward
    sub cx , 4
    push bx 
    push cx 
    call calculateDi
    mov dx ,word[es:di]
    cmp dx , ax 
    je retIs1
    jmp moveUpwards




moveDownwardsForPage1:
    cmp cx , 26
    jae restoreBxToMoveDownwardForPage2
    add cx , 4
    push bx 
    push cx 
    call calculateDi
    mov dx ,word[es:di]
    cmp dx , ax 
    je retIs1
    jmp moveDownwardsForPage1 



restoreBxToMoveDownwardForPage2:
    mov bx , [bp-2]
    mov cx , 2
    push ax 
    mov ax , 0xba00
    mov es , ax
    mov word[temp] , ax 
    pop ax 

    push bx 
    push cx 
    call calculateDi
    mov dx ,word[es:di]
    cmp dx , ax
    je retIs1

    jmp moveDownwards
      
    
restoreBxToMoveDownward:
    mov bx , [bp-2]
    push ax 
    mov ax , word[currentPage]
    mov word[temp] , ax 
    mov es , ax 
    pop ax 

moveDownwards:
    cmp word[temp] , 0xb900
    je moveDownwardsForPage1
    cmp cx , 14
    jae findSubgrid
    add cx , 4
    push bx 
    push cx 
    call calculateDi
    mov dx ,word[es:di]
    cmp dx , ax
    je retIs1
    jmp moveDownwards

findSubgrid:
    cmp word[currentPage] , 0xb900
    jne findSubgridInPage2
    cmp word[currentY] , 14
    jbe firstThreeGrid
    cmp word[currentY] , 22
    jbe secondThreeGrid


firstThreeGrid:   
    cmp word[currentX] , 28
    ja firstTwoGrid
    push ax
    mov bx , 1 
    push bx
    call checkSubgrids
    jmp retIs

firstTwoGrid:
    cmp word[currentX] , 46
    ja firstGrid
    push ax
    mov bx , 2 
    push bx
    call checkSubgrids
    jmp retIs
firstGrid:
    push ax
    mov bx , 3 
    push bx
    call checkSubgrids
    jmp retIs


secondThreeGrid:
    cmp word[currentX] , 28
    ja secondTwoGrid
    push ax
    mov bx , 4 
    push bx
    call checkSubgrids
    jmp retIs
secondTwoGrid:
    cmp word[currentX] , 46
    ja secondGrid
    push ax
    mov bx , 5 
    push bx
    call checkSubgrids
    jmp retIs
secondGrid:
    push ax
    mov bx , 6 
    push bx
    call checkSubgrids
    jmp retIs 

findSubgridInPage2:
    cmp word[currentY] , 2
    jbe secondThreeGridIn2
    cmp word[currentY] , 14
    jbe thirdThreeGridIn2
secondThreeGridIn2:
    cmp word[currentX] , 28
    ja secondTwoGridIn2
    push ax
    mov bx , 4 
    push bx
    call checkSubgrids
    jmp retIs
secondTwoGridIn2:
    cmp word[currentX] , 46
    ja secondGridIn2
    push ax
    mov bx , 5 
    push bx
    call checkSubgrids
    jmp retIs
secondGridIn2:
    push ax
    mov bx , 6 
    push bx
    call checkSubgrids
    jmp retIs
thirdThreeGridIn2:
    cmp word[currentX] , 28
    ja thirdTwoGridIn2
    push ax
    mov bx , 7 
    push bx
    call checkSubgrids
    jmp retIs
thirdTwoGridIn2:
    cmp word[currentX] , 46
    ja thirdGridIn2
    push ax
    mov bx , 8 
    push bx
    call checkSubgrids
    jmp retIs
thirdGridIn2:
    push ax
    mov bx , 9 
    push bx
    call checkSubgrids
    jmp retIs

validValue:
    mov si , 1 

retIs:
    
    pop di
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop cx
    pop bp
    ret 2 

checkSubgrids:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push di 
    mov ax, word[bp - 6]
    
    cmp word[bp - 4] , 1
    je subG1
    jmp comp2
   
retCheckSubgrids1:
    jmp retCheckSubgrids
subG1:
     mov bx , 0xb900
     mov es , bx
     mov di , 992
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
	 
     mov di , 1004
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 1016
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 1632
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 1644
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 1656
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 2272
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 2284
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids1
     mov di , 2296
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids2:
     jmp retCheckSubgrids     
comp2:    
     cmp word[bp - 4] , 2
     je subG2 
     jmp comp3
subG2:
     mov bx , 0xb900
     mov es , bx
     mov di , 1028
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 1040
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 1052
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 1668
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 1680
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 1692
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 2308
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 2320
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids2
     mov di , 2332
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids3:
     jmp retCheckSubgrids
comp3:    
     cmp word[bp - 4] , 3
     je subG3 
     jmp comp4
subG3:
     mov bx , 0xb900
     mov es , bx
     mov di , 1064
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 1076
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 1088
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 1704
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 1716
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 1728
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 2344
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 2356
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids3
     mov di , 2368
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids4:
     jmp retCheckSubgrids
comp4:
     cmp word[bp - 4] , 4
     je subG4 
     jmp comp5
subG4:
     mov bx , 0xb900
     mov es , bx
     mov di , 2912
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 2924
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 2936
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 3552
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 3564
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 3576
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov bx , 0xba00
     mov es , bx 
     mov di , 352
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 364
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids4
     mov di , 376
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids5:
     jmp retCheckSubgrids    
comp5:
     cmp word[bp - 4] , 5
     je subG5 
     jmp comp6
subG5:
     mov bx , 0xb900
     mov es , bx
     mov di , 2948
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 2960
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 2972
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 3588
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 3600
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 3612
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov bx , 0xba00
     mov es , bx 
     mov di , 388
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 400
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids5
     mov di , 412
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids6:
     jmp retCheckSubgrids 
comp6:
     cmp word[bp - 4] , 6
     je subG6 
     jmp comp7
subG6:
     mov bx , 0xb900
     mov es , bx
     mov di , 2984
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 2996
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 3008
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 3624
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 3636
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 3648
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov bx , 0xba00
     mov es , bx 
     mov di , 424
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 436
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids6
     mov di , 448
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids7:
     jmp retCheckSubgrids 
comp7:
     cmp word[bp - 4] , 7
     je subG7 
     jmp comp8
subG7:
     mov bx , 0xba00
     mov es , bx
     mov di , 992
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 1004
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 1016
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 1632
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 1644
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 1656
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 2272
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 2284
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids7
     mov di , 2296
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids8:
     jmp retCheckSubgrids 
comp8:
     cmp word[bp - 4] , 8
     je subG8
     jmp comp9
subG8:
     mov bx , 0xba00
     mov es , bx
     mov di , 1028
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 1040
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 1052
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 1668
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 1680
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 1692
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 2308
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 2320
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids8
     mov di , 2332
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids
retCheckSubgrids9:
     jmp retCheckSubgrids     
comp9:
     cmp word[bp - 4] , 9
     je subG9
     jmp retCheckSubgrids
subG9:  
     mov bx , 0xba00
     mov es , bx
     mov di , 1064
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 1076
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 1088
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 1704
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 1716
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 1728
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 2344
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 2356
     call checkElementInSubgrid
     cmp si , 0
     je retCheckSubgrids9
     mov di , 2368
     call checkElementInSubgrid
     cmp si , 0
     jmp retCheckSubgrids

retCheckSubgrids:
    pop di
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx
    pop bp
    ret 4


checkElementInSubgrid:
    push dx 
    mov dx , word[es:di] 
    cmp dx , ax 
    je foundElement
    mov si , 1 
    jmp retCheckElementInSubgrid
foundElement:
    mov si , 0
	
retCheckElementInSubgrid:
    pop dx
    ret  

removeValue:
    push ax
    push es 
    push di 
    push si 
    push bx 
    mov es , word[currentPage]
    mov di , word[current]
    
    cmp word[notes] , 1
    je skipUndoStorage2
    ; Storing undo values
    mov bx, word[currUndo]         ; Current undo stack pointer
    cmp bx, 180 * 2                ; Check if undoArr is full
    jae skipUndoStorage2            ; Skip if out of bounds
    mov dx, word[es:di]            ; Save current value at the addres

    mov word[undoArr + bx], dx     ; Store value in undoArr
    add bx, 2                      ; Increment pointer for next word
    mov cx, word[current]          ; Save current memory address
    mov word[undoArr + bx], cx     ; Store address in undoArr
    add bx, 2                      ; Increment pointer for next word
     mov word[undoArr + bx], es
     add bx , 2 
    mov word[currUndo], bx         ; Update undo pointer
    
    
    




skipUndoStorage2:

   
    mov bx , 0x720 
    or bh, 11110000b

    mov word[es:di-164] , bx 

    mov word[es:di-160] , bx

    mov word[es:di-156] , bx

    mov word[es:di-4] , bx


    mov word[es:di+4] , bx

    mov word[es:di+156] , bx
    mov word[es:di+160] , bx

    mov word[es:di+164] , bx



    mov word[es:di] , bx 


retRemove:
    pop bx 
    pop si
    pop di
    pop es
    pop ax 
    ret

addValues:
    jmp startAddValues
callAddNotes:
    call addNotes
    jmp retIncrementmistakes
retAdd1:
    jmp retAdd


startAddValues:
    push es 
    push di 
    push si 
    push bx 
    push cx
    push dx
    mov di , word[current]
    mov es , word[currentPage]

    cmp word[notes] , 1
    jz callAddNotes
    push ax 
    call isValidValuve
    cmp si , 0
    je retAdd1
    add word[score] , 50
	
    push si  
    call updateCard
   ; Storing undo values
    mov bx, word[currUndo]         ; Current undo stack pointer
    cmp bx, 180 * 2                ; Check if undoArr is full
    jae skipUndoStorage            ; Skip if out of bounds
    mov dx, word[es:di]            ; Save current value at the address
    mov word[undoArr + bx], dx     ; Store value in undoArr
    add bx, 2                      ; Increment pointer for next word
    mov cx, word[current]          ; Save current memory address
    mov word[undoArr + bx], cx     ; Store address in undoArr
    add bx, 2                      ; Increment pointer for next word
    mov word[undoArr+bx] , es 
    add bx ,2 
    mov word[currUndo], bx         ; Update undo pointer
skipUndoStorage:
    mov cx , 0xb900
    push cx
    mov cx, 244
    push cx
    push word [score]  
    call printnum
    
    mov bx , 0x720 
    or bh, 11110000b

    mov word[es:di-164] , bx 

    mov word[es:di-160] , bx

    mov word[es:di-156] , bx

    mov word[es:di-4] , bx

    mov word[es:di+4] , bx
	
    mov word[es:di+156] , bx
	
    mov word[es:di+160] , bx

    mov word[es:di+164] , bx
    mov ah , byte[currentGridColor] 
    mov word[es:di] , ax 
   
    jmp retAdd

retAdd:
    cmp si , 0
    je incrementmistakes
   
retIncrementmistakes:
    call printCards
    pop dx
    pop cx 
    pop bx 
    pop si
    pop di
    pop es
    ret


incrementmistakes:
    inc byte[mistake]
    mov cx , 0xb900
    push cx
    mov cx, 366
    push cx
    push word [mistake]  
    call printnum
     call play_custom_sound
    jmp retIncrementmistakes


updateCard:
    push bp 
    mov bp , sp 
    push ax 
    push bx 
    push si 


    cmp al, '1'                ; Compare AL with '1'
    je dec_card_1              ; If equal, jump to decrement card 1
    cmp al, '2'                ; Compare AL with '2'
    je dec_card_2              ; If equal, jump to decrement card 2
    cmp al, '3'                ; Compare AL with '3'
    je dec_card_3              ; If equal, jump to decrement card 3
    cmp al, '4'                ; Compare AL with '4'
    je dec_card_4              ; If equal, jump to decrement card 4
    cmp al, '5'                ; Compare AL with '5'
    je dec_card_5              ; If equal, jump to decrement card 5
    cmp al, '6'                ; Compare AL with '6'
    je dec_card_6              ; If equal, jump to decrement card 6
    cmp al, '7'                ; Compare AL with '7'
    je dec_card_7              ; If equal, jump to decrement card 7
    cmp al, '8'                ; Compare AL with '8'
    je dec_card_8              ; If equal, jump to decrement card 8
    cmp al, '9'                ; Compare AL with '9'
    je dec_card_9              ; If equal, jump to decrement card 9

    jmp end_decrement          ; If no match, skip decrement

dec_card_1:
    mov si , 0 
    jmp end_decrement

dec_card_2:
     mov si , 1
    jmp end_decrement

dec_card_3:
    mov si , 2
    jmp end_decrement

dec_card_4:
     mov si , 3 
    jmp end_decrement

dec_card_5:
    mov si , 4
    jmp end_decrement

dec_card_6:
    mov si , 5 
    jmp end_decrement

dec_card_7:
    mov si , 6 
    jmp end_decrement

dec_card_8:
    mov si , 7 
    jmp end_decrement

dec_card_9:
     mov si , 8

end_decrement:
    mov bl ,byte[noOFcards + si ]
    cmp word[bp+4] , 1
    je deccc
    inc bl
    jmp procecute 
deccc:
    dec  bl
procecute:
    mov byte[noOFcards + si] , bl 


    pop si 
    pop bx 
    pop ax 
    pop bp
    ret 2                      ; Return and clean up 2 bytes of parameters


; decrementCard:
;     pusha                       ; Save all general-purpose registers

;     mov bl ,byte[noOFcards] 
;     dec bl
;     mov byte[noOFcards] , bl 


;     sub al, '1'                 ; Convert character '1'-'9' to index 0-8
;     cmp al, 8                   ; Ensure AL is within valid range (0 to 8)
;     ja end_decrement            ; If out of range, skip decrement

;     mov si, ax                  ; Use AL (0-8) as the offset
;     mov bl, byte [noOFcards + si] ; Load the value from noOFcards + SI
;     dec bl                      ; Decrement the value
;     mov byte [noOFcards + si], bl ; Store the decremented value back

; end_decrement:
;     popa                        ; Restore all registers
;     ret 2                       ; Return and clean up 2 bytes of parameters




addNotes:
  jmp addNotesStart
 retAddNotes2:
  jmp retAddNotes


addNotesStart:
    push es 
    push di 
    push si 
    push ax
    
    push ax 
    call isValidValuve
    cmp si , 0 
    je retAddNotes2

    mov di , word[current]
    mov es , word[currentPage]
    cmp al , '1'
    je add1
    cmp al , '2'
    je add2
    cmp al , '3'
    je add3
    cmp al , '4'
    je add4
    cmp al , '5'
    je add5
    cmp al , '6'
    je add6
    cmp al , '7'
    je add7
    cmp al , '8'
    je add8
    cmp al , '9'
    je add9
    jmp retAddNotes
add1: 
    mov bx , word[es:di-164] 
    mov ah , 0x1F 
    mov word[es:di-164] , ax 
    jmp retAddNotes
add2: 
    mov bx , word[es:di-160] 
    mov ah , 0x1F 
    mov word[es:di-160] , ax
     jmp retAddNotes
add3: 
    mov bx , word[es:di-156] 
    mov ah , 0x1F 
    mov word[es:di-156] , ax
     jmp retAddNotes
add4: 
    mov bx , word[es:di-4] 
    mov ah , 0x1F 
    mov word[es:di-4] , ax
     jmp retAddNotes
add5:
    mov bx , word[es:di] 
    mov ah , 0x1F 
    mov word[es:di] , ax 
     jmp retAddNotes
add6: 
    mov bx , word[es:di+4] 
    mov ah , 0x1F 
    mov word[es:di+4] , ax
     jmp retAddNotes
add7: 
    mov bx , word[es:di+156] 
    mov ah , 0x1F 
    mov word[es:di+156] , ax
     jmp retAddNotes
add8: 
    mov bx , word[es:di+160] 
    mov ah , 0x1F 
    mov word[es:di+160] , ax
     jmp retAddNotes
add9: 
    mov bx , word[es:di+164] 
    mov ah , 0x1F 
    mov word[es:di+164] , ax


retAddNotes:
    pop ax       
    pop si
    pop di
    pop es
    ret

calculateNext:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 

    ; Move currentX and currentY into registers
    mov bx, [currentX]
    mov cx, [currentY]
    jmp compare 

restoreStart:
    mov bx, 16 
    mov cx, 6
    jmp updateXY

leftKey:
    
    cmp bx, 64          ; Compare currentX to 64
    jae o1              ; If currentX >= 64, jump to oi
    add bx, 6           ; Otherwise, add 6 to currentX
    jmp updateXY        ; Jump to updateXY

o1:
    cmp word[currentPage], 0xba00
    jne l1
    cmp cx, 14
    jae restoreStart
l1:   
    cmp cx, 22
    jae restoreStart
    mov bx, 16          ; Reset currentX to 16
    add cx, 4           ; Increment currentY by 4
    jmp updateXY        ; Jump to updateXY




compare:
    cmp byte[bp+4] , 'd'
    jz leftKey 
    cmp byte[bp+4] , 'a'
    jz rightKey
    cmp byte[bp+4] , 's'
    jz downKey
    cmp byte[bp+4] , 'w'
    jz upKey 
    jmp updateXY        ; Jump to updateXY
 


downKey:

    cmp word[currentPage], 0xba00
    je d1
    cmp cx, 22          ; Compare currentX to 64
    jae o3              ; If currentX >= 64, jump to oi
    add cx, 4           ; Otherwise, add 6 to currentX
    jmp updateXY        ; Jump to updateXY

o3:
    cmp bx, 64
    jae restoreStart
    mov cx, 6          ; Reset currentX to 16
    add bx, 6           ; Increment currentY by 4
    jmp updateXY        ; Jump to updateXY

d1: 
    cmp cx, 14
    jae d2
    add cx, 4           ; Otherwise, add 6 to currentX
    jmp updateXY        ; Jump to updateXY

d2:
    cmp bx, 64
    jae restoreStart
    mov cx, 2           ; Reset currentX to 16
    add bx, 6           ; Increment currentY by 4
    jmp updateXY        ; Jump to updateXY
   
rightRestoreStart:
    mov bx, 16 
    mov cx, 6
    jmp updateXY
	
rightKey:
    cmp bx, 16          ; Compare currentX to 16
    jbe o2              ; If currentX <= 16, jump to o2
    sub bx, 6           ; Otherwise, sub 6 to currentX
    jmp updateXY        ; Jump to updateXY
o2:
    cmp word[currentPage], 0xb900
    jne r1
    cmp cx, 6
    jbe rightRestoreStart
r1:    
    cmp cx, 3
    jbe restoreStart
    mov bx, 64          ; Reset currentX to 64
    sub cx, 4           ; Decrement currentY by 4
    jmp updateXY        ; Jump to updateXY

upKey:
    cmp word[currentPage], 0xba00
    je up1 
    cmp cx, 6           ; Compare currentX to 64
    jbe o4              ; If currentX >= 64, jump to oi
    sub cx, 4           ; Otherwise, add 6 to currentX
    jmp updateXY        ; Jump to updateXY

o4: 
    cmp bx, 16
    jbe rightRestoreStart
    mov cx, 22          ; Reset currentX to 16
    sub bx, 6           ; Increment currentY by 4
    jmp updateXY        ; Jump to updateXY
up1: 
    cmp cx , 2
    jbe up2
    sub cx , 4 
    jmp updateXY
up2: 
    cmp bx, 16
    jbe rightRestoreStart
    mov cx, 14          
    sub bx, 6           
    jmp updateXY 





updateXY:
    mov [currentX], bx  ; Update currentX in memory
    mov [currentY], cx  ; Update currentY in memory

    ; Calculate (80 * currentY + currentX) * 2
    mov ax, 80
    mul cx              ; AX = 80 * currentY
    add ax, bx          ; AX = 80 * currentY + currentX
    shl ax, 1           ; AX = (80 * currentY + currentX) * 2
    mov di, ax          ; Store result in DI
    ;mov [current], di   ; Update 'current' in memory

    ; Restore registers
    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret 2               ; Clean up 2 bytes from the stack on return

checkNumber:
    mov ch , 0 
    cmp cx , '9'
    jbe checkAgain
    mov dx , 0 
    jmp retCheck
checkAgain:
    cmp cx, '1'
    jae correctCheck
    mov dx , 0 
    jmp retCheck
correctCheck:
    mov dx , 1 
retCheck:
    ret 2             

checkWinCondition:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 

     call printCards
    
    mov si , 0xb900
    mov es , si

    mov dx , 0 
    mov ax, 16 
    mov bx, 6
seeNumber:
    push ax 
    push bx
    call calculateDi
    mov cx , word[es:di] 
    push cx 
    call checkNumber
    cmp dx , 0 
    jz retWin



    cmp ax, 64           ; Compare currentX to 64
    jae wi1              ; If currentX >= 64, jump to oi
    add ax, 6            ; Otherwise, add 6 to currentX
    jmp seeNumber        ; Jump to updateXY

wi1:
    cmp bx, 22
    jae checkWinInPage2
    mov ax, 16          ; Reset currentX to 16
    add bx, 4           ; Increment currentY by 4
    jmp seeNumber        ; Jump to updateXY



checkWinInPage2: 
    mov si , 0xba00 
    mov es , si
    mov dx , 0 
    mov ax, 16 
    mov bx, 2

seeNumber2:
    push ax 
    push bx
    call calculateDi
    mov cx , word[es:di] 
    push cx 
    call checkNumber
    cmp dx , 0 
    jz retWin



    cmp ax, 64           ; Compare currentX to 64
    jae wi2              ; If currentX >= 64, jump to oi
    add ax, 6            ; Otherwise, add 6 to currentX
    jmp seeNumber2        ; Jump to updateXY
    
     

wi2:   
    cmp bx, 14
    jae retWin
    mov ax, 16          ; Reset currentX to 16
    add bx, 4           ; Increment currentY by 4
    jmp seeNumber2        ; Jump to updateXY
    
retWin:
    cmp dx , 1
    je won 
    cmp word[mistake] , 5 
    je lost

    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret
	
lost:
	jmp gamelost
won: 
    jmp endgame


movCurrent:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx
    push es 
    push si 
    push di 
    mov ax , [bp+4]
    push ax 
    call calculateNext
    mov ax , [currentPage]
    mov es , ax

    push di 
    push bx 
    xor bx , bx 
box1:
    mov cx , word[es:di]
    or ch, 11110000b   ; Set the blinking bit and white background 
    mov word[es:di] , cx 

    add di , 2 
    mov cx , word[es:di]
    or ch, 11110000b   
    mov word[es:di] , cx 
    add di , 2 
    mov cx , word[es:di]
    or ch, 11110000b   
    mov word[es:di] , cx 

    sub di , 6 
    mov cx , word[es:di]
    or ch, 11110000b  
    mov word[es:di] , cx 
     sub di , 2 
    mov cx , word[es:di]
    or ch, 11110000b   
    mov word[es:di] , cx 
    add di , 4
    inc bx 
    cmp bx , 3 
    jae remain1
    cmp bx , 2 
    jae stk 
    add di , 160
    jmp box1 
stk: 
    sub di , 320 
    jmp box1    

remain1:
    pop bx 
    pop di 

    push di 
    push word[current]
    pop di 
    pop word[current]

    push di 
    push bx 
    xor bx , bx 
box2:
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7) 
    mov word[es:di] , cx 

    add di , 2 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 
    add di , 2 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 

    sub di , 6 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 
     sub di , 2 
    mov cx , word[es:di]
    and ch, 00001111b   ; Clear the 7th bit (bit 7)
    mov word[es:di] , cx 
    add di , 4
    inc bx 
    cmp bx , 3 
    jae remain2
    cmp bx , 2 
    jae stl
    add di , 160
    jmp box2 
stl: 
    sub di , 320 
    jmp box2    

remain2:
    pop bx 
    pop di     
    pop di
    pop si 
    pop es 
    pop dx
    pop ax 
    pop bx
    pop cx 
    pop bp
    ret 2


sleep:
	 push cx 
     mov cx, 0xFFFF 
delay:
	 loop delay 
     pop cx 
     ret

 printGrid:
    push bp             ; Save base pointer
    mov bp, sp          ; Set base pointer to stack pointer
    push cx             ; Save registers
    push bx
    push ax
    push dx

     mov ax , 0501h     ; show page 1
    int 10h 
    ; Load the color attribute passed from outside (if needed)
     mov ax, [bp+10]   ; The color attribute
     mov byte[currentGridColor] , al 

    ; Load the grid to print (easy, medium, or hard)
    mov dx, [bp+8]      ; The pointer to the grid to print is 8 bytes up the stack

    ; Load x position and y position
    mov cx, [bp+4]      ; Get y position
    mov bx, [bp+6]      ; Get x position

    ; Print top border
    mov ax, 0xb900
    push ax
    push bx
    push cx
    mov ax, 0x05        ; Default color attribute for the border
    push ax             ; Push color attribute
    mov ax, gridborder
    push ax
    call printStringWithSeg
    ; Loop to print rows and separator lines after every three rows
    mov si, 27          ; Total number of rows (9 rows in Sudoku)
    xor di, di          ; Row counter to track when to print a separator line

print_loop:
    ; Print the row
    cmp si , 12 
    jz screenEnd
special:
    cmp si , 12
    jbe change_ax
    mov ax , 0xb900
execute:
    push ax
    inc cx              ; Move to next row (y position)
    push bx
    push cx
    push word[bp+10]    ; Push color attribute (can modify for different colors if needed)
    mov ax, dx          ; Load the current row from the grid
    push ax
    call printStringWithSeg
    ;times 10 call sleep
    ; Increment the pointer to the next row
    add dx, 56         ; Move the pointer to the next row (assuming each row is 38 bytes long)

    ; Update the row counter and check if a separator line is needed
    inc di              ; Increment the row counter
    cmp di, 3           ; Check if 3 rows have been printed
    jne skip_separator  ; If not, skip printing the separator

    ; Print separator line
    cmp si , 12
    jbe change_ax2
    mov ax , 0xb900
execute2:
    push ax
    inc cx              ; Move to the next row (y position)
    push bx
    push cx
    mov ax, 0x07        ; Default color attribute for the separator line
    push ax             ; Push color attribute
    mov ax, gridborder
    push ax
    call printStringWithSeg

    xor di, di          ; Reset the row counter after printing the separator line

skip_separator:
    dec si              ; Decrement row counter (for 9 total rows)
    jnz print_loop      ; If more rows remain, repeat the loop

    ;call printUndoErase ; To print Undo and Erase
    push 0xb900
    call setCurrent
    ; Clean up the stack
	call prominentBorder
    pop dx
    pop ax
    pop bx
    pop cx
    pop bp
    ret 8               ; Return and clean up 8 bytes (color, grid, x, y)

change_ax :
    mov ax , 0xbA00
    jmp execute
change_ax2 :
    mov ax , 0xbA00
    jmp execute2
screenEnd :
    call EnableTimer
    ;times 150 call sleep  
    mov cx, 0           ; Get y position
    mov bx, [bp+6]      ; Get x position
    jmp special

clrscreen:
		push es
		push ax
		push di
		
		mov ax,0xb800
		mov es,ax
		mov di,0
		
nextloc:
		mov word [es:di],0x0720
		add di,2
		cmp di,4000
		jne nextloc
		
		pop di
		pop ax
		pop es
		ret
printStringWithSeg:
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si
		push di
		
		push ds
		pop es
		mov di,[bp+4]         
		mov cx,0xffff         
		xor al,al
		repne scasb           
		mov ax,0xffff
		sub ax,cx             
		dec ax                
		jz _end                
		
		mov cx,ax
		mov ax,[bp+12]         
		mov es,ax
		mov al,80             
		mul byte[bp+8]        
		add ax,[bp+10]        
		shl ax,1              
		mov di,ax             
		mov si,[bp+4]         
		mov ah,[bp+6]         
		cld                   
		
	next_char:
		lodsb               
		cmp al, '|'           
		jnz normalChar       ; If not '|', jump to normal_char

		; Handle special case for '|'
		push ax               ; Save current character (|)
		mov ah, 0x07         ; Set a different color (e.g., 0x1F is white on blue)
		stosw                 ; Store character and color in video memory
		pop ax                ; Restore the original character from the stack
		jmp afterChar        ; Skip restoring the original color yet

	normalChar:
		stosw                 ; Store character and original color in video memory
        

	afterChar:
		loop next_char         
		
	_end:
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10                 


printstring:
		push bp
		mov bp,sp
		push es
		push ax
		push cx
		push si
		push di
		
		push ds
		pop es
		mov di,[bp+4]         
		mov cx,0xffff         
		xor al,al
		repne scasb           
		mov ax,0xffff
		sub ax,cx             
		dec ax                
		jz end                
		
		mov cx,ax
		mov ax,0xb800         
		mov es,ax
		mov al,80             
		mul byte[bp+8]        
		add ax,[bp+10]        
		shl ax,1              
		mov di,ax             
		mov si,[bp+4]         
		mov ah,[bp+6]         
		cld                   
		
	nextchar:
		lodsb               
		cmp al, '|'           
		jnz normal_char       ; If not '|', jump to normal_char

		; Handle special case for '|'
		push ax               ; Save current character (|)
		mov ah, 0x07         ; Set a different color (e.g., 0x1F is white on blue)
		stosw                 ; Store character and color in video memory
		pop ax                ; Restore the original character from the stack
		jmp after_char        ; Skip restoring the original color yet

	normal_char:
		call sleep
		;call sleep
		;call sleep
		stosw                 ; Store character and original color in video memory
        

	after_char:
		loop nextchar         
		
	end:
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 8                 

wait_for_key:
	mov ah, 00h      ; Function to read a key
    int 16h          ; Call BIOS
    ret

draw_border_end:
    push es
    push ax
    push di

    mov ax, 0xbb00
    mov es, ax

    ; Draw top-left corner
    mov di, 0
    mov al, 201        ; '╔'
    mov ah, 0x30	   ;attribute byte
    stosw

    ; Draw top border
    mov cx, 78
top_border_loop_end:
    mov al, 205        ; '═'
    stosw
    loop top_border_loop_end

    ; Draw top-right corner
    mov al, 187        ; '╗'
    stosw

    ; Draw vertical borders
    mov di, 160        ; Next row
    mov cx, 23
vertical_border_loop_end:
    mov al, 186        ; '║'
    stosw
    add di, 158        ; Move to the next row, first column
    loop vertical_border_loop_end

    ; Draw bottom-left corner
    mov al, 200        ; '╚'
    stosw

    ; Draw bottom border
    mov cx, 78
bottom_border_loop_end:
    mov al, 205        ; '═'
    stosw
    loop bottom_border_loop_end

    ; Draw bottom-right corner
    mov al, 188        ; '╝'
    stosw

    ; Draw right vertical border
    mov di, 160
    mov cx, 23
right_vertical_border_loop_end:
    add di, 158
    mov al, 186        ; '║'
    stosw
    loop right_vertical_border_loop_end

    pop di
    pop ax
    pop es
    ret
clrscreen2:
    push es
    push ax
    push cx
    push di

    mov ax, 0xbb00        ; Set ES to video memory (text mode)
    mov es, ax
    xor di, di            ; Start at the beginning of the screen
    mov ax, 0x3020        ; Blue background, space character
    mov cx, 2000          ; Clear the entire screen (25 rows, 80 columns)
    cld
    rep stosw             ; Fill video memory with the blue background and space characters

    pop di
    pop cx
    pop ax
    pop es
    ret
	
fill_box_score:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xbb00            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (4 * 80 + 30) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_score:
        push di
        mov cx, 20               ; Width of the filling area (51 - 44)
    fill_columns_score:
        mov al, 0x20            ; Space character
        mov ah, 0x07            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_score
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_score

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret
	
fill_box_time:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xbb00            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (8 * 80 + 30) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_time:
        push di
        mov cx, 20               ; Width of the filling area (51 - 44)
    fill_columns_time:
        mov al, 0x20            ; Space character
        mov ah, 0x07            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_time
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_time

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret
	
fill_box_msg:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xbb00            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (12 * 80 + 25) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_msg:
        push di
        mov cx, 30               ; Width of the filling area (51 - 44)
    fill_columns_msg:
        mov al, 0x20            ; Space character
        mov ah, 0x07            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_msg
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_msg

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret
printEndWindow:
    push bp
    mov bp, sp
    push ax
    push cx
    push dx

    mov ax , 0503h ; Show page 3
    int 10h 

	call clrscreen2
	call draw_border_end
    call DisableTimer

	call fill_box_score
	; Set the position and color
    mov ax , 0xBB00
    push ax 
    mov ax, 32       ; X position 
    push ax
    mov ax, 5       ; Y position 
    push ax
    mov ax, 3        ; Color attribute (Blue on white)
    push ax
    mov ax, totalscore
    push ax
    call printStringWithSeg
	
	mov cx , 0xbb00
    push cx
    mov cx, 890		;Calculating the position using the formula (ypos*80+xpos)*2
    push cx
    push word [score]  
    call printnum
	
	call fill_box_time
	; Set the position and color
    mov ax , 0xbb00
    push ax 
    mov ax, 32       ; X position (10)
    push ax
    mov ax, 9       ; Y position (12)
    push ax
    mov ax, 3        ; Color attribute (Cyan)
    push ax
    mov ax, timetaken
    push ax
    call printStringWithSeg  
    
    mov cx , 0xbb00
    push cx 
    mov cx, 1528
    push cx
    push word [cs:minute]  
    call printnum
    
    mov cx , 0xbb00
    push cx
    mov cx, 1534
    push cx
    push word [cs:second]  
    call printnum
	
	call fill_box_msg
    ; Set the position and color
    mov ax , 0xbb00
    push ax 
    mov ax, 27       ; X position (10)
    push ax
    mov ax, 13       ; Y position (12)
    push ax
    mov ax, 3        ; Color attribute (Cyan)
    push ax
    mov ax, endmessage
    push ax
    call printStringWithSeg
    
    ; Wait for another key press before exiting
    call wait_for_key

    pop dx
    pop cx
    pop ax
    pop bp
    ret
delaylostsound:
	pusha
	mov cx,0xFFFF
	looplostsound:
		dec cx
		jnz looplostsound
	popa
	ret

set_frequency_lostsound:
    ; Sends frequency to PIT channel 2
    out 0x42, al
    mov al, ah            
    out 0x42, al          

    in al, 0x61           
    or al, 0x03         
    out 0x61, al          

    ret

play_lost_sound:
    ; Set up PIT channel 2 in mode 3 (square wave)
    mov al, 0xB6          
    out 0x43, al         

    ; High pitch tone (~1000 Hz)
    mov ax, 1193          ; Divider for ~1000 Hz
    call set_frequency_lostsound
    call delaylostsound

    ; Low pitch tone (~150 Hz)
    mov ax, 7680          ; Divider for ~150 Hz
    call set_frequency_lostsound
    call delaylostsound

    ; Very high pitch tone (~1200 Hz)
    mov ax, 1016          ; Divider for ~1200 Hz
    call set_frequency_lostsound
    call delaylostsound

    ; Medium pitch tone (~300 Hz)
    mov ax, 3570          ; Divider for ~300 Hz
    call set_frequency_lostsound
    call delaylostsound

    ; Very low pitch tone (~100 Hz)
    mov ax, 11992         ; Divider for ~100 Hz
    call set_frequency_lostsound
    call delaylostsound

    ; Final rapid high pitch tone (~1500 Hz)
    mov ax, 809           ; Divider for ~1500 Hz
    call set_frequency_lostsound
    call delaylostsound

    ; Turn off speaker
    in al, 0x61          
    and al, 11111100b     ; Clear bits 0 and 1 (disable speaker)
    out 0x61, al          ; Write back to port 0x61

    ret

printlostwindow:
	push bp
    mov bp, sp
    push ax
    push cx
    push dx

    mov ax , 0503h ; Show page 3
    int 10h 

	mov cx,3
looptiloop_lostsound:
    call play_lost_sound
	dec cx
	jnz looptiloop_lostsound
turnoffspeakers_lost:
    in al, 0x61            ; Read port 0x61
    and al, 11111100b      ; Clear bits 0 and 1 (disable speaker)
    out 0x61, al           ; Write back to port 0x61
	call clrscreen2
	call draw_border_end
    call DisableTimer

	call fill_box_score
	; Set the position and color
    mov ax , 0xBB00
    push ax 
    mov ax, 32       ; X position 
    push ax
    mov ax, 5       ; Y position 
    push ax
    mov ax, 3        ; Color attribute (Blue on white)
    push ax
    mov ax, totalscore
    push ax
    call printStringWithSeg
	
	mov cx , 0xbb00
    push cx
    mov cx, 890		;Calculating the position using the formula (ypos*80+xpos)*2
    push cx
    push word [score]  
    call printnum
	
	call fill_box_time
	; Set the position and color
    mov ax , 0xbb00
    push ax 
    mov ax, 32       ; X position (10)
    push ax
    mov ax, 9       ; Y position (12)
    push ax
    mov ax, 3        ; Color attribute (Cyan)
    push ax
    mov ax, timetaken
    push ax
    call printStringWithSeg  
    
    mov cx , 0xbb00
    push cx 
    mov cx, 1528
    push cx
    push word [cs:minute]  
    call printnum
    
    mov cx , 0xbb00
    push cx
    mov cx, 1534
    push cx
    push word [cs:second]  
    call printnum
	
	call fill_box_msg
    ; Set the position and color
    mov ax , 0xbb00
    push ax 
    mov ax, 29       ; X position (10)
    push ax
    mov ax, 13       ; Y position (12)
    push ax
    mov ax, 3        ; Color attribute (Cyan)
    push ax
    mov ax, lostmessage
    push ax
    call printStringWithSeg
    
    ; Wait for another key press before exiting
    call wait_for_key

    pop dx
    pop cx
    pop ax
    pop bp
    ret
draw_border:
    push es
    push ax
    push di

    mov ax, 0xb800
    mov es, ax

    ; Draw top-left corner
    mov di, 0
    mov al, 201        ; '╔'
    mov ah, 0x30	   ;attribute byte
    stosw

    ; Draw top border
    mov cx, 78
top_border_loop:
    mov al, 205        ; '═'
    stosw
    loop top_border_loop

    ; Draw top-right corner
    mov al, 187        ; '╗'
    stosw

    ; Draw vertical borders
    mov di, 160        ; Next row
    mov cx, 23
vertical_border_loop:
    mov al, 186        ; '║'
    stosw
    add di, 158        ; Move to the next row, first column
    loop vertical_border_loop

    ; Draw bottom-left corner
    mov al, 200        ; '╚'
    stosw

    ; Draw bottom border
    mov cx, 78
bottom_border_loop:
    mov al, 205        ; '═'
    stosw
    loop bottom_border_loop

    ; Draw bottom-right corner
    mov al, 188        ; '╝'
    stosw

    ; Draw right vertical border
    mov di, 160
    mov cx, 23
right_vertical_border_loop:
    add di, 158
    mov al, 186        ; '║'
    stosw
    loop right_vertical_border_loop

    pop di
    pop ax
    pop es
    ret

fill_box_easy:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xb800            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (9 * 80 + 12) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_easy:
        push di
        mov cx, 12               ; Width of the filling area (51 - 44)
    fill_columns_easy:
        mov al, 0x20            ; Space character
        mov ah, 0x70            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_easy
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_easy

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret	

fill_box_med:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xb800            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (9 * 80 + 35) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_med:
        push di
        mov cx, 12               ; Width of the filling area (51 - 44)
    fill_columns_med:
        mov al, 0x20            ; Space character
        mov ah, 0x70            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_med
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_med

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret	

fill_box_hard:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xb800            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (9 * 80 + 58) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_hard:
        push di
        mov cx, 12               ; Width of the filling area (51 - 44)
    fill_columns_hard:
        mov al, 0x20            ; Space character
        mov ah, 0x70            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_hard
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_hard

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret	
fill_box_welcome:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xb800            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (2 * 80 + 28) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 3                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_welcome:
        push di
        mov cx, 25               ; Width of the filling area (51 - 44)
    fill_columns_welcome:
        mov al, 0x20            ; Space character
        mov ah, 0x70            
        stosw                   ; Write the character to video memory
        loop fill_columns_welcome
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_welcome

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret		
fill_box_instructions:
    push es
    push ax
    push di
    push cx
    push bx

    mov ax, 0xb800            ; Set ES to video memory (text mode)
    mov es, ax

    ; Start filling from (10, 44) to (11, 50)
    mov di, (14 * 80 + 3) * 2   ; Set DI to the first character inside the box (y=10, x=44)
    mov bx, 9                   ; Height of the filling area (11 - 9 - 1)
    fill_rows_instructions:
        push di
        mov cx, 74               ; Width of the filling area (51 - 44)
    fill_columns_instructions:
        mov al, 0x20            ; Space character
        mov ah, 0x70            ; White text on blue background
        stosw                   ; Write the character to video memory
        loop fill_columns_instructions
        pop di
        add di, 160             ; Move to the next row inside the box (80 columns * 2 bytes per character)
        dec bx
        jnz fill_rows_instructions

    pop bx
    pop cx
    pop di
    pop ax
    pop es
    ret	
	

clrscreen1:
    push es
    push ax
    push cx
    push di

    mov ax, 0xb800        ; Set ES to video memory (text mode)
    mov es, ax
    xor di, di            ; Start at the beginning of the screen
    mov ax, 0x3020        ; Blue background, space character
    mov cx, 2000          ; Clear the entire screen (25 rows, 80 columns)
    cld
    rep stosw             ; Fill video memory with the blue background and space characters

    pop di
    pop cx
    pop ax
    pop es
    ret
titleScreen :

    push ax 
	call clrscreen1
	
    
    call draw_border
	call fill_box_welcome
	;call draw_message_border
	;Printing the start message
    mov ax , 0xb800
    push ax 
	;push x position
	mov ax,32
	push ax
	;push y position
	mov ax,3
	push ax
	;Pushing the attribute byte
	mov ax,0x70
	push ax
	mov ax,startmessage
	push ax
	call printStringWithSeg
	
	;Printing the level message
	;push x position
	mov ax,26
	push ax
	;push y position
	mov ax,6
	push ax
	;Pushing the attribute byte
	mov ax,0x30
	push ax
	mov ax,chooselevelsmessage
	push ax
	call printstring
	
	
	;Printing the instruction message
	;push x position
	mov ax,18
	push ax
	;push y position
	mov ax,7
	push ax
	;Pushing the attribute byte
	mov ax,0x30
	push ax
	lea ax,[instructionmessage]
	push ax
	call printstring
	
	call fill_box_easy
	;Printing the level message
	;push x position
	mov ax,16
	push ax
	;push y position
	mov ax,10
	push ax
	;Pushing the attribute byte
	mov ax,0x70
	push ax
	mov ax,easy
	push ax
	call printstring
	
	call fill_box_med
	;push x position
	mov ax,38
	push ax
	;push y position
	mov ax,10
	push ax
	;Pushing the attribute byte
	mov ax,0x70
	push ax
	lea ax,[medium]
	push ax
	call printstring
	
	call fill_box_hard
	;push x position
	mov ax,62
	push ax
	;push y position
	mov ax,10
	push ax
	;Pushing the attribute byte
	mov ax,0x70
	push ax
	lea ax,[hard]
	push ax
	call printstring
	
	call fill_box_instructions
	;Printing the level message
	;push x position
	mov ax,35
	push ax
	;push y position
	mov ax,14
	push ax
	;Pushing the attribute byte
	mov ax,0x75
	push ax
	mov ax,instructions
	push ax
	call printstring
	
	;push x position
	mov ax,6
	push ax
	;push y position
	mov ax,15
	push ax
	;Pushing the attribute byte
	mov ax,0x71
	push ax
	mov ax,movement_message
	push ax
	call printstring
	
	;push x position
	mov ax,6
	push ax
	;push y position
	mov ax,16
	push ax
	;Pushing the attribute byte
	mov ax,0x71
	push ax
	mov ax,pagenumber
	push ax
	call printstring
	
	;push x position
	mov ax,6
	push ax
	;push y position
	mov ax,17
	push ax
	;Pushing the attribute byte
	mov ax,0x71
	push ax
	mov ax,undorule
	push ax
	call printstring
	
	;push x position
	mov ax,6
	push ax
	;push y position
	mov ax,18
	push ax
	;Pushing the attribute byte
	mov ax,0x71
	push ax
	mov ax,eraserule
	push ax
	call printstring
	;push x position
	mov ax,6
	push ax
	;push y position
	mov ax,19
	push ax
	;Pushing the attribute byte
	mov ax,0x71
	push ax
	mov ax,endscreen
	push ax
	call printstring
	
	;push x position
	mov ax,6
	push ax
	;push y position
	mov ax,20
	push ax
	;Pushing the attribute byte
	mov ax,0x71
	push ax
	mov ax,notesmessage
	push ax
	call printstring
	
	mov cx, 8            ; Number of repetitions
looptiloop:
    call play_custom_sound
    dec cx
    jnz looptiloop
    pop ax 
    ret

printGridInstructions :

    push bp 
    mov bp , sp 
    cmp byte[currentLevel] , 1
    je loadEasy
    cmp byte[currentLevel], 2
    je loadMedium
loadHard:
    mov bx , hardCards
    jmp startPrintGridInstructions
loadEasy:
    mov bx , easyCards
     jmp startPrintGridInstructions
loadMedium:
    mov bx , mediumCards
    jmp startPrintGridInstructions

startPrintGridInstructions   
    mov cx , 0 
    mov si , 0 
    mov di , 0
    CopyLoop:
        mov al, byte[bx+si]         ; Load byte from source into AL
        mov byte[noOFcards+di], al         ; Store byte from AL into destination
        inc si               ; Increment source pointer
        inc di               ; Increment destination pointer
        inc cx 
        cmp cx, 9            ; Check if end of string (null terminator)
        jne CopyLoop         ; If not null terminator, repeat loop



    mov ax, 0xb900
    push ax
    push ax 
    ;Printing the score message on the grid window 
	;push x position
	mov ax,36
	push ax
	;push y position
	mov ax,1
	push ax
	;Pushing the attribute byte
	mov ax,10
	push ax
	mov ax,score_message
	push ax
	call printStringWithSeg
	
	;Printing the mistake message on the grid window 
    mov ax, 0xb900
    push ax
	;push x position
	mov ax,14
	push ax
	;push y position
	mov ax,2
	push ax
	;Pushing the attribute byte
	mov ax,3
	push ax
	mov ax,mistakes_message
	push ax
	call printStringWithSeg
    
    ;Printing the level difficulty message on the grid window 
    mov ax, 0xb900
    push ax
	;push x position
	mov ax,37
	push ax
	;push y position
	mov ax,2
	push ax
	;Pushing the attribute byte
	mov ax,3
	push ax
	mov ax,[bp + 4]
	push ax
	call printStringWithSeg

	;Printing the time message on the grid window 
    mov ax, 0xb900
    push ax
	;push x position
	mov ax,57
	push ax
	;push y position
	mov ax,2
	push ax
	;Pushing the attribute byte
	mov ax,3
	push ax
	mov ax,time_message
	push ax
	call printStringWithSeg
    pop bp
    pop ax



    ret 2


; printUndoErase :
;     push ax 

; 	;Printing the undo message on the grid window 
;     mov ax , 0xba00 ; Set the page no 1 
;     push ax  ; push the page no
; 	;push x position
; 	mov ax,13
; 	push ax
; 	;push y position
; 	mov ax,19
; 	push ax
; 	;Pushing the attribute byte
; 	mov ax,3
; 	push ax
; 	mov ax,undo_message
; 	push ax
; 	call printStringWithSeg
	
; 	;Printing the erase message on the grid window 
;     mov ax , 0xba00 ; Set the page no 1 
;     push ax  ; push the page no
; 	;push x position
; 	mov ax,62
; 	push ax
; 	;push y position
; 	mov ax,19
; 	push ax
; 	;Pushing the attribute byte
; 	mov ax,3
; 	push ax
; 	mov ax,erase_message
; 	push ax
; 	call printStringWithSeg
;     pop ax 
;     ret

start:
    
    mov ax , 03h
    int 10h

    call titleScreen

    mov ah, 00h
    int 16h

    cmp al, '1'
    je load_easy
    cmp al, '2'
    je load_medium
    cmp al, '3'
    je load_hard
    jmp endgame

load_easy:
    mov byte[currentLevel] , 1
    push easy       ; Push Level
    call printGridInstructions ;To print Grid Instruction such as Score, Time Taken, Difficulty Level

    ; Set up arguments for printGrid (easy level)
    mov ax, 0x0E   ; Color attribute (set to some color, e.g., 0x0E for yellow)
    push ax          ; Push color attribute
    mov ax, easy_gridrow1 ; Address of easy grid
    push ax          ; Push address of the easy grid
    mov ax, 13       ; X position 
    push ax          ; Push x position
    mov ax, 4       ; Y position 
    push ax          ; Push y position
    call printGrid   ; Call the printGrid function
    jmp l

load_medium:
    mov byte[currentLevel] , 2
	push medium      ; Puch Level
    call printGridInstructions  ;To print Grid Instruction such as Score, Time Taken, Difficulty Level    
    ; Set up arguments for printGrid (medium level)
    mov ax, 0x0A     ; Color attribute (set to some color, e.g., 0x0A for green)
    push ax          ; Push color attribute
    mov ax, medium_gridrow1 ; Address of medium grid
    push ax          ; Push address of the medium grid
    mov ax, 13       ; X position 
    push ax          ; Push x position
    mov ax, 4        ; Y position 
    push ax          ; Push y position
    call printGrid   ; Call the printGrid function
    jmp l

load_hard:
    mov byte[currentLevel] , 3
	push hard                   ; Puch Level
    call printGridInstructions  ;To print Grid Instruction such as Score, Time Taken, Difficulty Level
    ; Set up arguments for printGrid (hard level)
    mov ax, 0x0C     ; Color attribute (set to some color, e.g., 0x0C for red)
    push ax          ; Push color attribute
    mov ax, hard_gridrow1 ; Address of hard grid
    push ax          ; Push address of the hard grid
    mov ax, 13       ; X position 
    push ax          ; Push x position
    mov ax, 4        ; Y position 
    push ax          ; Push y position
    call printGrid   ; Call the printGrid function
    jmp l

showPage1:
    push 0xb900
    call setCurrent
    mov ax , 0501h
    int 10h
    jmp l
showPage2:
    push 0xba00
    call setCurrent
    mov ax , 0502h
    int 10h 
    push 0xba00
    call setCurrent
    jmp l 

l:  
    call checkWinCondition
    mov ah, 00h
    int 16h
    cmp al, 'c'            ; Check if it’s the Up Arrow key (scan code 0x48)
    jz showPage1           ; Jump to showPage1 if Up Arrow key was pressed
    cmp al, 'v'            ; Check if it’s the Down Arrow key (scan code 0x50)
    jz showPage2           ; Jump to showPage2 if Down Arrow key was pressed
    cmp al, 0x1B            ; Check if the Escape key (ASCII 0x1B) was pressed
    jz endgame             ; Jump to exit_loop if Escape was pressed
    cmp al, 'd'
    jz callMoveCurrent  
    cmp al, 'a'
    jz callMoveCurrent
    cmp al, 's'
    jz callMoveCurrent
    cmp al, 'w'
    jz callMoveCurrent
    cmp al, '0'
    jz callRemoveValue
    cmp al, 'n'
    jz switchNotes
    cmp al, '9'
    jbe addCheck
    cmp al, 'u'
    je callPerformUndo

    jmp l                  ; Otherwise, keep waiting for a key
callMoveCurrent:
    push ax
    call movCurrent
    jmp l
addCheck:
    cmp al , '1'
    jae callAddValues
    jmp l
callAddValues:
    call addValues
    jmp l
switchNotes:
    cmp word[notes] , 0 
    jz OnNotes
    mov word[notes] , 0
    call removepencil
    jmp l
OnNotes:
    mov word[notes] , 1
   call printpencil
    jmp l 
callRemoveValue:
    call removeValue
    jmp l 
callPerformUndo:
    call performUndo
    jmp l

gamelost:
	
	call printlostwindow
endgame:
   
    call printEndWindow ; Display the end window
kill:
    mov ax, 0x4c00 ; Exit the program
    int 21h
delaysound:
    pusha
    mov cx, 0xFFFF
loopsound:
    dec cx
    jnz loopsound
    popa
    ret

set_frequency:
    ; Sends frequency to PIT channel 2
    out 0x42, al
    mov al, ah            
    out 0x42, al          

    in al, 0x61           
    or al, 0x03         
    out 0x61, al          

    ret

play_custom_sound:
    ; Set up PIT channel 2 in mode 3 (square wave)
    mov al, 0xB6          
    out 0x43, al        

    ; Melody: C4 (261 Hz), E4 (329 Hz), G4 (392 Hz), C5 (523 Hz)
    
    ; C4 tone (~261 Hz)
    mov ax, 4572          ; Divider for ~261 Hz
    call set_frequency
    call delaysound

    ; E4 tone (~329 Hz)
    mov ax, 3628          ; Divider for ~329 Hz
    call set_frequency
    call delaysound

    ; G4 tone (~392 Hz)
    mov ax, 3041          ; Divider for ~392 Hz
    call set_frequency
    call delaysound

    ; C5 tone (~523 Hz)
    mov ax, 2285          ; Divider for ~523 Hz
    call set_frequency
    call delaysound

    ; Turn off the speaker
    in al, 0x61          
    and al, 0xFC          
    out 0x61, al        

    ret

    

	
removepencil:
	mov ax , 0xb900
    mov es , ax 
	mov di,162
	loop_pencil:
    mov word[es:di] , 0x0720
	add di,2
	cmp di,174
	jne loop_pencil
	
	mov ax , 0xba00
    mov es , ax 
	mov di,162
	loop_pencil1:
    mov word[es:di] , 0x0720
	add di,2
	cmp di,174
	jne loop_pencil1
	
	ret

printpencil:

	mov ax , 0xb900 ; Set the page no 1 
    push ax  ; push the page no
	mov ax,1
	push ax
	;push y position
	mov ax,1
	push ax
	;Pushing the attribute byte
	mov ax,3
	push ax
	mov ax,pencil
	push ax
	call printStringWithSeg
	
	mov ax , 0xba00 ; Set the page no 2 
    push ax  ; push the page no
	mov ax,1
	push ax
	;push y position
	mov ax,1
	push ax
	;Pushing the attribute byte
	mov ax,3
	push ax
	mov ax,pencil
	push ax
	call printStringWithSeg
    ret
    

