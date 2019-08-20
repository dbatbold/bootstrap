#!/bin/sh

# Image map:
# mbr.bin    offset 0, size 1 block
# mbr.bin    offset 1, size 1 block (backup copy)
# editor.bin offset 2, size 15 blocks

set -e  # exit on error

DEP="qemu-system-x86_64 qemu-img"

if ! which $DEP >/dev/null; then
	echo "Missing required packages."
	echo "for Debian, run:"
	echo " $ apt install qemu-kvm qemu-utils"
	exit 1
fi

# first drive
dd if=/dev/zero of=sda count=100

# install bootloader
dd if=mbr.bin of=sda conv=notrunc

# install backup bootloader
dd if=mbr.bin of=sda seek=1 conv=notrunc

# install editor
if test -e ../editor/editor.bin; then
	dd if=../editor/editor.bin of=sda seek=2 count=15 conv=notrunc
fi

# second drive
dd if=/dev/zero of=sdb count=100

# start virtual machine
qemu-system-x86_64 \
	-enable-kvm \
	-nographic \
	-monitor pty \
	-drive format=raw,file=sda \
	-drive format=raw,file=sdb

	# -display none \
	# -curses \
	# -cpu host \
	# -enable-kvm \
