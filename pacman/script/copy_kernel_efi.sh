#! /bin/sh
#
# Copy kernel and micro-code images into EFI directory

EFI_DIR="/efi/EFI/arch"

cp -af /boot/vmlinuz-linux "$EFI_DIR/"
[ -e /boot/intel-ucode.img ] && cp -af /boot/intel-ucode.img "$EFI_DIR/"

exit 0
