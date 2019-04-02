#!/bin/sh

DEP="qemu-system-x86_64 qemu-img"

if ! which $DEP >/dev/null; then
	echo "Missing required packages."
	echo "for Ubuntu, run:"
	echo " $ apt install qemu qemu-kvm qemu-utils qemu-system-x86"
	exit 1
fi

# first drive
dd if=/dev/zero of=sda count=10

# install bootloader
dd if=mbr.bin of=sda conv=notrunc

# second drive
dd if=/dev/zero of=sdb count=10

# start virtual machine
qemu-system-x86_64 \
	-cpu host \
	-enable-kvm \
	-drive format=raw,file=sda \
	-drive format=raw,file=sdb
