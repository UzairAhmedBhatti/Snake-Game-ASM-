


[org 0x0100]
jmp start
awaz:
mov cx,5
mov si,0
awaz1:
mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
		mov ax,[awazain+si]
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 5          ; Pause for duration of note.
pause1:
        mov     dx, 65535
pause2:
        dec     dx
        jne     pause2
        dec     bx
        jne     pause1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.7
		add si,2
loop awaz1
ret
time:
inc word[cs:seconds]
mov di,142
push di
push word[cs:seconds]
call printtime
mov di,136
push di
push word[cs:minute]
call printtime
cmp word[cs:seconds],60
jne nochangeinm
inc word[cs :minute]
mov word[cs:seconds],0
nochangeinm:
mov word [cs:tickcount],0	
ret

changes:
inc word[cs:anspeed]
cmp word[cs:anspeed], 20
jne toend
mov word[cs:go], 0
sub word[cs:speed], 1
mov word[cs:anspeed], 0
toend:
;mov word[cs:tickcount], 0
ret
living:
mov bp, sp
mov bx, [bp+2]
mov ax, 0xb800
mov es, ax
mov si, 0
mov dx, [cs:remain]
life:
mov word[es:si],dx
add si, 2
dec bx
cmp bx, 0
jne life
ret 2
 



clrscr:
	push es
	push ax
	push cx
	push di
	mov ax, 0xb800
	mov es, ax
	xor di, di
	mov ax, 0x0720
	mov cx, 2000 										
	cld 
	rep stosw
	pop di
	pop cx
	pop ax
	pop es
	ret
edges:
		mov ax, 0xb800
		mov es, ax
		mov di, 480
		mov al , '*'
		mov ah , 0x03
	l1:
		mov word [es:di], ax
		add di , 2
		cmp di ,638
		jbe l1
		mov di , 3360
	l2:	
		mov word [es:di], ax
		add di , 2
		cmp di ,3520
		jb l2

		mov di , 480
	l3:
		mov word [es:di], ax
		add di , 160
		cmp di , 3520
		jb l3

		mov di , 638
	l4:
		mov word [es:di], ax
		add di , 160
		cmp di , 3520
		jb l4
		ret
printsnake:
		mov ax, 0xb800
		mov es, ax
		mov si,[cs:arr]
		mov di,0
		mov ax,[cs:sanke]
		mov bx,[cs:startt]
		mov cx,[sizes]
		mov word [es:si], bx
		dec cx
loop1:
		add di , 2
		mov di,[cs:arr+di]
        mov word [es:si], ax
		loop loop1
		ret
		
kbisr:
in al,0x060
upp:
   cmp al, 0x48  ;up
   jne downn
   ;cmp word[cs:down],1
   ;je norm
   mov word[cs:up],1
	mov word[cs:down],0
	mov word[cs:left],0
	mov word[cs:right],0 
	jmp norm

downn:
     cmp al,0x50    ;down
     jne leftt
	 cmp word[cs:up],1
	 je norm
	mov word [cs:up],0
	mov word[cs:down],1
	mov word[cs:left],0
	mov word[cs:right],0
	jmp norm
leftt:
    cmp al,0x4b   ;left
	jne rightt
	cmp word[cs:right],1
	je norm
	mov word[cs:left],1
	mov word[cs:right],0
	mov word[cs:up],0
	mov word[cs:down],0
    jmp norm
rightt:
	cmp al,0x4d     ;right
	jne  notkey
	cmp word[cs:left],1
	je norm
	mov word[cs:right],1
	mov word [cs:up],0
	mov word[cs:down],0
	mov word[cs:left],0
    jmp norm
notkey:
jmp far[cs:oldisr]
norm:
 mov al,0x20
 out 0x20,al
 iret
newfruit:
push 0xb800
pop es
mov di,[cs:position]
mov bx,[cs:fruit]
mov word[es:di],bx
cmp word[cs:position],3600
jle gowithit
mov word[cs:position],900
gowithit:
ret
chechiffruiteaten:
push 0xb800
pop es
mov ax,[cs:fruit]
mov di,[cs:arr]
mov bx,[es:di]
cmp bx,ax
jne nofruiteaten
add word[cs:position],390
mov ax,[cs:fruit]
add al,1
add ah,2
mov word[cs:fruit],ax
add word[cs:sizes],4
nofruiteaten:
ret
originalsnake:
mov word[cs:sizes],10
mov cx,[cs:sizes]
mov si,0
backup:
mov dx,[cs:original+si]
mov word[cs:arr+si],dx
add si,2
loop backup
mov word[cs:right],1
mov word[cs:up],0
mov word[cs:down],0
mov word[cs:left],0
ret
timer:
    ;inc word[cs:tickcount]
	; inc word[cs:increasespeed]
	; cmp word[cs:increasespeed],180
	; jne proceedfurther
	; sub word[cs:speed],3
	; mov word[cs:increasespeed],0
	proceedfurther:
	mov si,[cs:speed]
	mov word[cs:timeinc], 0
	;call time
	inc word [cs:addd]
	cmp word[cs:addd],si
    jne exit1
	mov word[cs:addd], 0
	call clrscr
	inc word[cs:timeinc]
	cmp word[cs:timeinc], 19
	jne forww
	forww:
	mov word[cs:timeinc], 0
	call time
	call edges
	call checkedges
	push word[cs:num]
	call living
	call normal
    call changes
	call newfruit
	call chechiffruiteaten
s:

cmp word[cs:right],1
jne ahadr
call shift 
add word[cs:arr],2	
jmp exit1
ahadr:
cmp word[cs:left],1	
jne ahadl
call shift
sub word[cs:arr],2
jmp exit1
ahadl:
cmp word[cs:up],1
jne ahadu
call shift
sub word[cs:arr],160
jmp exit1
ahadu:	
cmp word[cs:down],1
jne exit1
call shift
add word[cs:arr],160		
	
exit1:
call normal
mov al,0x20
out 0x20,al
iret




normal:
mov ax,0xb800
mov es,ax
mov ax,[cs:sanke]
mov cx,[cs:sizes]
mov bx, 0
mov di,[cs:arr]
lll:
mov word[es:di],ax
add bx, 2
mov di, [cs:arr+bx]

loop lll
ret

shift:
	mov cx,[cs:sizes]
	sub cx,1
	mov di,0
	mov dx,[cs:arr+di]

ll:
	mov ax,[cs:arr+di+2]
	add di,2
	mov word[cs:arr+di],dx
	mov dx,ax
	loop ll
    ret
	
printtime:
     push bp
	 mov  bp, sp 
	 push es
	 push ax 
	 push bx
	 push cx
	 push dx
	 
    mov  ax, 0xb800 
	mov  es, ax             ; point es to video base    
	mov  ax, [bp+4]         ; load number in ax  
	mov  bx, 10             ; use base 10 for division      
	mov  cx, 0              ; initialize count of digits
	
nextdigit:
    mov  dx, 0              ; zero upper half of dividend  
	div  bx                 ; divide by 10 
	add  dl, 0x30           ; convert digit into ascii value
	push dx                 ; save ascii value on stack
	inc  cx                 ; increment count of values
	cmp  ax, 0              ; is the quotient zero    
	jnz  nextdigit          ; if no divide it again 
	
      mov  di,[bp+6]      ; point di to 70th column  
nextpos:    
  pop  dx                 ; remove a digit from the stack     
  mov  dh, 0x07           ; use normal attribute 
  mov  [es:di], dx        ; print char on screen    
  add  di, 2              ; move to next screen location  
  loop nextpos            ; repeat for all digits on stack
  
              pop  dx
			  pop  cx  
			  pop  bx 
              pop  ax 
	          pop es
			  pop bp
			  ret 4
checkedges:
mov dl,'*'
mov dh,0x03
mov ax,0xb800
mov es,ax
mov di,[cs:arr]
mov ax,[es:di]
cmp dx,ax
jne nn
call originalsnake
dec word[cs:num]
cmp word[cs:num],0
jle end
nn:
ret;

start:
    mov dh,0x07
	mov dl,':'
	mov word[cs:startt],dx
	call clrscr
	call edges
    mov word[cs:right],1
	xor ax,ax
	mov es,ax
	mov ax,[es:9*4]
	mov  word[oldisr],ax
	mov ax,[es:9*4+2]
	mov  word[oldisr+2],ax
	cli
	mov word[es:9*4],kbisr
	mov word[es:9*4+2],cs
	mov word[es:8*4],timer
	mov word[es:8*4+2],cs
	sti

end:
call awaz	
mov dx,start
add dx,15
mov cl,4
shr dx,cl
mov ax,0x3100
int 0x21


sanke: dw 0x072a
sizes: dw 10
arr: dw 682,680,678,676,674,672,670,668,666,664,662,660
original: dw 682,680,678,676,674,672,670,668,666,664,662,660
oldisr :dd 0
up:dw 0
down: dw 0
left: dw 0
right:dw 0
increasespeed: dw 0
startt: dw 0
speed: dw 10
tickcount:dw 0
remain: dw 0701
minute: dw 0
seconds: dw 0
num: dw 5
go: dw 0
anspeed: dw 0
addd: dw 0
timeinc: dw 0
fruit: dw 0x0904
position: dw 900
awazain: dw 1355,1521,1436,1715,2873