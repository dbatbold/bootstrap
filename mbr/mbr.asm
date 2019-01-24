; DL - receives boot drive number from BIOS

use16				; 16-bit code
org 0x7c00			; the address that BIOS loads MBR

	mov ax,ax
	mov sp,ax		; base of stack memory

	mov ax,0x8000	; start of stack memory
	mov ss,ax

start:
	call prompt		; prompt for a command
	push ax
	call to_hex_char
	mov al,ah
	call print_char	; print AL in hex
	pop ax
	call print_char	; print AH in hex
	jmp start

prompt:
	; read key press to AL
	xor ah,ah
	int 16h
	ret

to_hex_char:
	; converts AL value to hex characters into AX
	mov ah,al
	shr ah,4		; high 4-bit of hex
	cmp ah,9
	jg .high_hex
	add ah,'0'
	jmp .high_done
	.high_hex:
	add ah,'a'-10
	.high_done:
	and al,0fh		; low 4-bit of hex
	cmp al,9
	jg .low_hex
	add al,'0'
	ret
	.low_hex:
	add al,'a'-10
	ret

print_char:
	; teletype AL to screen
	mov ah,0eh
	xor bh,bh		; page 0
	mov bl,0f1h		; white on light gray
	int 10h
	ret

times 510-($-$$) db 0	; pad to 512 bytes

dw 0xaa55			; MBR signature for BIOS
