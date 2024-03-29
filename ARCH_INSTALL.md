## Arch install

setup overview:

- UEFI - GPT
- bootloader: systemd-boot
- initramfs gen: Booster
- ESP mounted to `/efi`
- ROOT type `ext4` (`btrfs` could be considered in next installs)
- Swap partition of 8G
- KMS

### Prerequisites

In UEFI firmware:

- ⚠ set the hardware clock to UTC time
- disable secure boot

### Partitioning

Partition the disk using `fdisk /dev/nvme0n1`

1. create a new GPT table
2. create 1st partition of 512M → ESP
3. change ESP type to "EFI system"
4. create 2nd partition of -8G (= remaining size for swap) → ROOT
5. ROOT type should be set by default to "Linux filesystem"
6. create 3rd part with the remaining space (8G) → SWAP
7. change SWAP type to "Linux swap"
8. write the disk & exit fdisk

→ https://wiki.archlinux.org/title/Partitioning

### Formatting

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

→ https://wiki.archlinux.org/title/EFI_system_partition \
→ https://wiki.archlinux.org/title/File_systems

#### Mounting FS

1. mount ROOT

```
mount /dev/nvme0n1p2 /mnt
```

2. mount ESP to `/efi`

```
mount --mkdir /dev/nvme0n1p1 /mnt/efi
```

### follow Arch wiki steps

from https://wiki.archlinux.org/title/Installation_guide#Installation

then **chroot**

### Booster

```
pacman -S booster
```

Initramfs images will be generated and located under `/boot`
directory.

### KMS

Edit `/etc/booster.yaml`

For intel GPU

```
modules_force_load: i915
```

For nvidia GPU

```
modules_force_load: nvidia nvidia_modeset nvidia_uvm nvidia_drm
```

sources:

- https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start
- https://wiki.archlinux.org/title/Booster#Early_module_loading
- https://wiki.archlinux.org/title/NVIDIA#Early_loading

### systemd-boot

```
bootctl install
```

write the conf loader in `/efi/loader/loader.conf`

```
default arch.conf
timeout 4
console-mode keep
editor no
```

→ https://man.archlinux.org/man/loader.conf.5#OPTIONS

write the Arch Linux entry in `/efi/loader/entries/arch.conf`

```
title   Arch Linux
linux   /vmlinuz-linux
initrd  /intel-ucode.img
initrd  /booster-linux.img
options root=LABEL=ARCH rw quiet splash
```

For nvidia add `nvidia_drm.modeset=1` to kernel parameters

```
options root=LABEL=ARCH rw quiet splash nvidia_drm.modeset=1
```

#### copy boot file to ESP

```
cp -a /boot/vmlinuz-linux /efi/
cp -a /boot/booster-linux.img /efi/
cp -a /boot/intel-ucode.img /efi/
```

⚠ be sure to read [system maintenance](#-system-maintenance)

### POST install

#### network

check interfaces

```
ip link
```

The ether interface should be DOWN by default\
To have it UP at boot use systemd-networkd.service & systemd-resolvd.service\
enable/start these services

→ https://wiki.archlinux.org/title/Network_configuration \
→ https://wiki.archlinux.org/title/Systemd-networkd

#### ⚠ system maintenance

To upgrade `systemd-boot` on package upgrade add the
following custom pacman hook in `/etc/pacman.d/hooks/`

```
[Trigger]
Type = Package
Operation = Upgrade
Target = systemd

[Action]
Description = Upgrading systemd-boot ⚡
When = PostTransaction
Exec = /usr/bin/systemctl restart systemd-boot-update.service
```

To automatically copy kernel, initramfs and microcode images into
the ESP after a system upgrade add this pacman hook

```
[Trigger]
Type = Path
Operation = Install
Operation = Upgrade
Target = usr/lib/modules/*/vmlinuz
Target = usr/lib/booster/*
Target = boot/*-ucode.img

[Action]
Description = Copying boot files to EFI ⚡
When = PostTransaction
Exec = /usr/local/bin/boot-files-copy.sh
```

Add the following script into `/usr/local/bin/`

```
#! /bin/sh

ESP="/efi/"

cp -a /boot/vmlinuz-linux $ESP
cp -a /boot/booster-linux.img $ESP
cp -a /boot/intel-ucode.img $ESP

exit 0
```

#### time & time sync

→ https://wiki.archlinux.org/title/System_time \
→ https://wiki.archlinux.org/title/Systemd-timesyncd

Check the current settings

```
timedatectl
```

The hardware clock (`RTC time` in the output) should match the current UTC time \
Set the timezone

```
$ timedatectl list-timezones
# timedatectl set-timezone Europe/Paris
```

Enable time synchronization

```
# systemctl enable systemd-timesyncd.service
# systemctl start systemd-timesyncd.service
# timedatectl set-ntp true
```

#### user

→ https://wiki.archlinux.org/title/Users_and_groups#User_management

```
useradd -Um -G wheel -s /bin/fish pierre
passwd pierre
```

#### TRIM

→ https://wiki.archlinux.org/index.php/Solid_state_drive#Periodic_TRIM

```
# systemctl enable fstrim.timer
# systemctl start fstrim.timer
```

#### pacman stuff

→ https://github.com/doums/dotfiles/tree/master/pacman
