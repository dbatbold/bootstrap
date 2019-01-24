#!/bin/sh

DEP="fasm"

if ! which $DEP >/dev/null; then
	echo "Missing a required package, run:"
	echo " $ apt install fasm"
	exit 1
fi

fasm mbr.asm
