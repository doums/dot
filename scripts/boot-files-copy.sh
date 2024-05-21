#! /bin/sh
#
# Copy kernel, initramfs images to EFI directory
# Set ESP variable to EFI system partition mount point
#

ESP="/efi/"

cp -a /boot/vmlinuz-linux $ESP
cp -a /boot/initramfs-linux.img $ESP

exit 0
