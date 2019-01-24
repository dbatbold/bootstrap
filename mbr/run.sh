#!/bin/sh

DEP="qemu-system-x86_64 qemu-img kvm"

if ! which $DEP >/dev/null; then
	echo "Missing required packages."
	echo "for Ubuntu, run:"
	echo " $ apt install qemu qemu-kvm qemu-utils qemu-system-x86"
	exit 1
fi

qemu-system-x86_64 \
	-m 1G \
	-cpu host \
	-enable-kvm \
	-display sdl \
	-drive format=raw,file=mbr.bin
