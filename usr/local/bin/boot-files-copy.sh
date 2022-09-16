#! /bin/sh
#
# Copy kernel, initramfs images and microcode to EFI directory
# Set ESP variable to EFI system partition mount point
#

ESP="/efi/"

cp -a /boot/vmlinuz-linux $ESP
cp -a /boot/initramfs-linux.img $ESP
cp -a /boot/initramfs-linux-fallback.img $ESP
cp -a /boot/intel-ucode.img $ESP

exit 0
