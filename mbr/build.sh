#!/bin/sh

ASM="fasm nasm yasm"

rm mbr.bin 2>/dev/null || true

for asm in $ASM; do
	if which $asm >/dev/null; then
		if [ $asm = fasm ]; then
			CMD="$asm mbr.asm"
		else
			CMD="$asm -o mbr.bin mbr.asm"
		fi
		break
	fi
done

if [ -z "$CMD" ]; then
	echo "On of '$ASM' not found."
else
	echo $CMD
	$CMD
fi
