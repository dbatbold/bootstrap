; x86 bootloader for bootstrap
;
; The code generates 512-byte Master Boot Rector (MBR)
; for booting x86 based PCs, and prompts the user to run
; read, edit, write and execute commands on disk blocks.
;
; To build, install one of FASM, YASM, NASM assembler, and run:
; $ ./build.sh
;
; The resulting code can be viewed with following commands:
; $ objdump -m i8086 -D -b binary mbr.bin
; $ hexdump -vC mbr.bin
;
; To test, install QEMU virtual machine, and run:
; $ ./run.sh
;
; DL - register receives the boot drive number from BIOS
; 0x7e00 - address of free memory can be used
;
; Command format:
;  rBBBBCCCC - read CCCC blocks from block address BBBB
;  eBBBB     - edit 0x9000 block buffer starting from offset BBBB
;  wBBBBCCCC - write CCCC blocks from 0x9000 to block address BBBB
;  x         - execute code in block buffer (0x9000)
;
; Rules:
;  - Hex address and block count must be entered in lower case [0-9][a-f])
;  - Hex numbers are not validated for correctness due to code size limit
;  - Only supports reading and writing the boot drive
;  - Reading and writing MBR block is not allowed
;
; Examples:
;  >r0001000f - Reads 16 blocks starting from the second sector
;               to memory address 0x9000.
;  >e000f     - Edit bytes starting from address 0x900f (offset 15)
;  >w0002001e - Writes 30 blocks starting from the third sector on boot drive.
;  >x         - Execute code starting from address 0x9000.
;
; Memory map:
;  0x7e00  16 bytes - command input buffer
;  0x7e10   2 bytes - BBBB command address parsed from input buffer
;  0x8000   2 bytes - CCCC number of blocks parsed from input buffer
;  0x8002   2 bytes - byte hex buffer used for editor
;  0x8004   1 bytes - boot drive number (from DL)
;  0x8fff           - bottom of the stack memory
;  0x9000 64K bytes - block buffer for read, edit, write and execute commands

BASE		equ 0x7c00	; segment offset that MBR is loaded
BBBB		equ 0x7e10	; command address
CCCC		equ 0x8000	; number of blocks
HEXBUF		equ 0x8002	; hex buffer
DRIVE		equ 0x8004	; boot drive number passed from BIOS
STACK		equ 0x8fff	; beginning of stack memory (grows upward)
BUFFER		equ 0x9000	; buffer for disk read, write and execute commands

use16				; 16-bit code
org BASE
	
	; setup stack memory
	xor ax,ax		; clear AX
	mov ss,ax		; clear stack segment
	mov sp,STACK		; set stack pointer

	call init

times 13-($-$$) nop		; align offset to the previous version on tape

start:
	call print_new_line
	mov al,'>'		; prompt for a command
	call print_char
	call read_commnad
	call run_command
	jmp start

read_commnad:
	; read entered command into buffer
	push bx
	xor bx,bx
  .read_char:
	call read_keypress
	cmp al,08h		; Backspace key
	jne .is_full
	cmp bx,0
	je .read_char
	call print_char
	mov al,' '
	call print_char		; delete last char
	mov al,08h
	call print_char		; move cursor
	dec bx
	mov byte [0x7e00+bx],0	; delete last char from buffer
	;call print_command
	jmp .read_char
  .is_full:
	cmp bx,16
	je .read_char		; buffer is full
	cmp al,0dh		; Enter key
	je .done
	mov [0x7e00+bx],al
	inc bx
	call print_char
	jmp .read_char
  .done:
	pop bx
	ret

; Removed because no space left in MBR block
; print_command:
; 	push bx
; 	call print_new_line
; 	mov al,'>'		; prompt for a command
; 	call print_char
; 	mov bx,0
;  .next_char:
; 	mov al,[0x7e00+bx]
; 	cmp al,0		; end of string
; 	je .done
; 	cmp bx,15		; buffer is full
; 	je .done
; 	call print_char
; 	inc bx
; 	jmp .next_char
;  .done:
; 	pop bx
; 	ret

print_new_line:
	push ax
	mov al,0ah 		; new line
	call print_char
	mov al,0dh
	call print_char
	pop ax
	ret

read_keypress:
	; read key press to AL
	mov ah,0
	int 16h
	ret

hex_to_string:
	; converts AL value to hex ascii characters into AX
	mov ah,al		; copy AL
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

string_to_hex:
	; converts ascii string in AX to ascii code in AL
	push bx
	xor bx,bx
	cmp al,'9'
	jg .low_hex
	sub al,'0'
	jmp .check_high
  .low_hex:
	sub al,'a'-10
  .check_high:
	add bl,al
	cmp ah,'9'
	jg .high_hex
	sub ah,'0'
	jmp .done
  .high_hex:
	sub ah,'a'-10
  .done:
	shl ah,4
	add bl,ah
	mov al,bl
	pop bx
	ret

print_char:
	; prints AL to TTY
	push ax
	push bx
	mov ah,0eh
	mov bh,0		; page 0
	mov bl,0f1h		; white on light gray
	int 10h
	pop bx
	pop ax
	ret

print_byte_hex:
	; prints AL value to TTY in hex format
	push ax
	call hex_to_string
	xchg ah,al		; print AH
	call print_char
	xchg ah,al		; print AL
	call print_char
	pop ax
	ret

run_command:
	push bx
	xor bx,bx
	call parse_command_address
	call parse_command_block_count
	mov al,[0x7e00]
	cmp al,'e'		; edit blocks
	jne .is_read
	call edit_block_buffer
   .is_read:
	cmp al,'r'		; read blocks
	jne .is_write
	mov ax,4200h
	call read_or_write_blocks
	jmp .done
  .is_write:
	cmp al,'w'		; write blocks
	jne .is_exec
	mov ax,4300h
	call read_or_write_blocks
;	call write_blocks
	jmp .done
  .is_exec:
	cmp al,'x'		; execute code in block buffer
	jne .done
	call print_new_line
	call BUFFER
  .done:
	pop bx
	ret

parse_command_address:
	; parse address (BBBB) from command input buffer into [0x7e10]
	mov ax,[0x7e01]		; high address digits
	xchg ah,al
	call string_to_hex
	mov [0x7e11],al		; save parsed high address
	mov ax,[0x7e03]		; low address digits
	xchg ah,al
	call string_to_hex
	mov [BBBB],al		; save parsed low address
	ret

parse_command_block_count:
	; parse block count (CCCC) from command input buffer into [0x8000]
	mov ax,[0x7e05]		; high address digits
	xchg ah,al
	call string_to_hex
	mov [0x8001],al		; save parsed high address
	mov ax,[0x7e07]		; low address digits
	xchg ah,al
	call string_to_hex
	mov [CCCC],al		; save parsed low address
	ret

edit_block_buffer:
	; Allow user to edit bytes starting address 0x9000 with
	; word offset in 0x7e10.
	push bx
	push cx
	call print_new_line
	mov bx,[BBBB]

	; print command address
  .print_offset:
	xor cx,cx
	mov ax,bx
	xchg ah,al
	call print_byte_hex
	xchg ah,al
	call print_byte_hex
	mov al,' '
	call print_char

	; print offset
	mov al,[BUFFER+bx]
	call print_byte_hex
	mov al,' '
	call print_char

	; prompt for 2 hex digits until enter key
  .prompt_hex:
	call read_keypress
	cmp al,0dh		; Enter key
	je .enter_key
	cmp cx,0
	jne .high_hex
	mov [0x8003],al
	jmp .next_char
  .high_hex:
	mov [HEXBUF],al
  .next_char:
	inc cx
	cmp cx,3		; last byte?
	jne .print_key
	mov cx,1		; save to first char
	push ax
	mov [0x8003],al
	mov al,08h		; Backspace key
	call print_char
	call print_char
	pop ax
  .print_key:
	call print_char
	jmp .prompt_hex

  .enter_key:
	cmp cx,0
	je .done
	mov ax,[HEXBUF]
	call string_to_hex
	mov [BUFFER+bx],al
	call print_new_line
	inc bx			; move to next byte
	xor cx,cx
	jmp .print_offset

  .done:
	pop cx
	pop bx
	ret

read_or_write_blocks:
 	; Read/Write CCCC blocks from/to address BBBB. IO buffer starts at 0x9000.
 	; AH: 42h read blocks
 	; AH: 43h write blocks
 	;   AL: bit 0 = 0: close write check
 	;   AL: bit 0 = 1: open write check
 	;   AL: bit 1-7: reserved, set to 0
 
 	push bx
 	mov bx,[BBBB]		; BBBB block address
 
 	; push ax
 	; mov al,bh
 	; call print_byte_hex
 	; mov al,bl
 	; call print_byte_hex
 	; pop ax
 
 	; protect from overwriting MBR block
 	cmp bx,0		; is MBR?
 	jne .write
;	jne .build_dap
 	mov al,'M'
 	call print_char
 	jmp .done
 
 	; build Disk Address Packet (DAP) in stack
;  .build_dap:
; 	push dword 0		; ^                                  (+4)
; 	push word 0		; |                                  (+2)
; 	push word bx		; LBA 8-byte address                 (+2)
; 	push word 0x0000	; buffer memory segment              (+2)
; 	push word BUFFER	; buffer memory offset               (+2)
; 	push word [CCCC]	; number of sectors to read/write    (+2)
; 	push word 0x1000	; DAP struct size (plus unused byte) (+2)
; 	mov si,sp		; store DAP address                  (=16)

   .write:
	push ax
	mov ax,[CCCC]
	mov [DAP_NUM_SECTORS],ax
	mov [DAP_LBA],bx
	pop ax
 	mov si,DAP		; address of DAP
 	mov dl,[DRIVE]		; disk number
 	int 13h
; 	add sp,0x10		; remove DAP from stack
 	jnc .done
  .print_result:
 	call print_new_line
 	xchg ah,al
 	call print_byte_hex	; print error code
  .done:
 	pop bx
 	ret

; write sectors to disk use CHS address
; write_blocks:
; 	push ax
; 	push bx
; 	push cx
; 	push dx
; 
; 	xor ax,ax
; 	mov es,ax		; buffer segment
; 	mov bx,BUFFER		; buffer offset
; 	mov ch,0		; cylinder (0-1023)
; 	mov cl,2		; sector (1-63)
; 	mov dh,0		; head (0-15)
; 	mov dl,[DRIVE]		; drive
; 	mov al,1		; number of sectors
; 	mov ah,03h		; write to disk
; 	int 13h
;  	jnc .done
;  	call print_new_line
;  	xchg ah,al
;  	call print_byte_hex	; print error code
; 
;   .done:
; 	pop dx
; 	pop cx
; 	pop bx
; 	pop ax
; 	ret

init:
	; initialize segments
	;cli			; disable hardware interrupts
	cld			; clear direction flag
	mov [DRIVE],dl		; save boot drive number
	mov ds,ax		; clear data segment
	mov si,ax		; clear source index
	mov es,ax		; clear extra segment
	sti			; enable interrupts
	ret

DAP:
	db	0x10		; DAP data size (16)
	db	0		; unused
DAP_NUM_SECTORS:
	dw	0		; number of sectors to read/write
DAP_BUFFER:
	dw	BUFFER		; transfer buffer offset to read/write
	dw	0		; transfer buffer segment
DAP_LBA:
	dd	0		; LBA address (0-31 bits)
	dd	0		; LBA address (32-63 bits)

times 510-($-$$) db 0		; pad to 512 bytes

dw 0xaa55			; MBR signature for BIOS
