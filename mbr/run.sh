#!/bin/sh

DEP="qemu-system-x86_64 qemu-img"

if ! which $DEP >/dev/null; then
	echo "Missing required packages."
	echo "for Ubuntu, run:"
	echo " $ apt install qemu qemu-kvm qemu-utils qemu-system-x86"
	exit 1
fi

qemu-system-x86_64 \
	-cpu host \
	-enable-kvm \
	-drive format=raw,file=mbr.bin
