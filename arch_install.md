## Arch install

setup overview:

- UEFI - GPT
- bootloader: systemd-boot
- ESP mounted to `/efi`
- ROOT type `ext4` (btrfs could be considered in next installs)
- Swap partition of 8G

### memo

#### Partitioning

Partition the disk using `fdisk /dev/nvme0n1`

1. create a new GPT table
2. create 1st partition of 512M -> ESP
3. change ESP type to "EFI system"
4. create 2nd partition of -8G (= remaining size for swap) -> ROOT
5. ROOT type should be set by default to "Linux filesystem"
6. create 3rd part with the remaining space (8G) -> SWAP
7. change SWAP type to "Linux swap"
8. write the disk & exit fdisk

#### Formatting

1. format ESP in `FAT32`

```
mkfs.fat -n ESP -F 32 /dev/nvme0n1p1
```

2. format ROOT in `ext4`

```
mkfs.ext4 -L ARCH /dev/nvme0n1p2
```

3. init SWAP

```
mkswap -L SWAP /dev/nvme0n1p3
swapon /dev/nvme0n1p3
```

#### Mounting FS

1. mount ROOT

```
mount /dev/nvme0n1p1 /mnt
```

2. mount ESP to `/efi`

```
mount --mkdir /dev/nvme0n1p1 /mnt/efi
```

#### follow Arch wiki steps

from https://wiki.archlinux.org/title/Installation_guide#Installation

### systemd-boot

Once **chrooted**, to install systemd-boot run

```
bootctl install
```

write the conf loader in `/efi/loader/loader.conf`

```
default arch.conf
timeout 4
console-mode 1
editor no
```

source: https://man.archlinux.org/man/loader.conf.5#OPTIONS

write the Arch Linux entry in `/efi/loader/entries/arch.conf`

```
title   Arch Linux
linux   /vmlinuz-linux
initrd  /intel-ucode.img
initrd  /initramfs-linux.img
options root=LABEL=ARCH rw quiet splash
```

TODO: add note for nvidia kernel modules

#### copy boot file to ESP

```
cp -a /boot/vmlinuz-linux /efi/
cp -a /boot/initramfs-linux.img /efi/
cp -a /boot/intel-ucode.img /efi/
```

TODO add instruction for auto updates

### POST install

to setup wired connection, use systemd-networkd & systemd-resolvd

enable/start these services
