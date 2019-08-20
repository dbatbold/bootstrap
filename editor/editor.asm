; 'editor.asm' is a text editor that can be loaded by 'mbr' as below:
;
; >d0080     - change device (boot device)
; >r0002000f - read 15 blocks starting from the second sector
; >x         - execute 'editor.bin'
;
; Memory map:
; 0x07c00
;    512B mbr.bin
; 0x07dff 
; 0x07e00 
;    28KB MBR reserved memory
; 0x0ffff
; 0x10000
;   480KB Free memory
; 0x7ffff 

use16
org 0x9000

	call	clear_screen
	;mov	ax,0102h
	;call	enable_cursor
	mov	ax,0000h
	call	move_cursor
start:
	call	print_page
   .read_key:
	call	read_keypress	; key code -> AL
	cmp	al,1bh		; is ESC
	je	start
	cmp	al,0dh		; is ENTER
	jne	.edit
	mov	al,0ah		; convert 0xd to 0xa
	call	print_char
	mov	al,0dh
   .edit:
   	cmp	[cursor_pos],0ffffh	; check if memory is full
	jne	.store
	call	print_new_line
	mov	ax,out_of_memory
	call	print_string
	jmp	.read_key
   .store:			; store pressed key code (AL) to memory
	mov	si,[cursor_pos]
	push	[buffer_seg]
	pop	es
	mov	byte [es:si],al
	call	print_page
	;call	print_char
	inc	[cursor_pos]	; increment cursor
	inc	[buffer_size]	; increment file size
	jmp	.read_key
   	
	; call	 read_commnad
	; call	 print_new_line
	; jmp	 start

print_page:
	push	cx si
	mov	ax,0000h
	call	move_cursor
	mov	cx,[page_size] 	; page counter
	xor	si,si
	push	[buffer_seg]
	pop	es
   .next:
   	cmp	si,[buffer_size]
	je	.done
	mov	al,byte [es:si]
	cmp	al,0ah		; new line?
	jne	.print
	call	print_new_line
	loop	.next
	jmp	.next_char
   .print:
	call	print_char
   .next_char:
	inc	si
	jmp	.next
   .done:
   	pop	si cx
	ret

print_new_line:
	push	ax
	mov	al,0ah 		; new line
	call	print_char
	mov	al,0dh
	call	print_char
	pop	ax
	ret

read_keypress:
	; read key press to AL
	mov	ah,0
	int	16h
	ret

print_char:
	; prints AL to TTY
	push	ax bx
	mov	ah,0eh
	mov	bh,0		; page 0
	mov	bl,0f1h		; white on light gray
	int	10h
	pop	bx ax
	ret

print_string:
	push	bx si
	mov	bh,0		; page 0
	mov	bl,0f1h		; light gray on black
	mov	si,ax
   .next:
	mov	al,[si]
	cmp	al,0
	je	.done
	mov	ah,0eh
	int	10h
	inc	si
	jmp	.next
   .done:
	pop	si bx
	ret

clear_screen:
	mov	al,03		; 80x25 16-bit color
	mov	ah,00
	int	10h
	ret

enable_cursor:
	; AL - start scanline
	; AH - end scanline
	push	cx
	mov	ch,ah
	mov	cl,al
	mov	ah,01h
	int	10h
	pop	cx
	ret
	
move_cursor:
	; AH - row
	; AL - column
	push	dx
	mov	dh,ah
	mov	dl,al
	mov	ah,02h
	int	10h
	pop	dx
	ret

print_byte_hex:
	; prints AL value to TTY in hex format
	push	ax
	call	hex_to_string
	xchg	ah,al		; print AH
	call	print_char
	xchg	ah,al		; print AL
	call	print_char
	pop	ax
	ret

hex_to_string:
	; converts AL value to hex ascii characters into AX
	mov	ah,al		; copy AL
	shr	ah,4		; high 4-bit of hex
	cmp	ah,9
	jg	.high_hex
	add	ah,'0'
	jmp	.high_done
  .high_hex:
	add	ah,'a'-10
  .high_done:
	and	al,0fh		; low 4-bit of hex
	cmp	al,9
	jg	.low_hex
	add	al,'0'
	ret
  .low_hex:
	add	al,'a'-10
	ret

cursor_pos	dw 0		; cursor position
buffer_size	dw 0		; file buffer size
buffer_seg	dw 1000h	; file buffer segment
page_size	dw 10		; number of lines on a page
out_of_memory	db 'memory is full',0
