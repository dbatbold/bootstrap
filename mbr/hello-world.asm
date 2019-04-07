; write 'Hello world!' to TTY

org 9000h

	push bx
	push si
	mov bh,0		; page 0
	mov bl,0f1h		; light gray on black
	mov si,hello
   .next:
	mov al,[si]
	cmp al,0
	je .done
	mov ah,0eh
	int 10h
	inc si
	jmp .next
   .done:
	pop si
	pop bx
	ret

hello:	db 'Hello world!', 0
