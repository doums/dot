## Arch install

setup overview:

- UEFI - GPT
- bootloader: systemd-boot
- initramfs: mkinitcpio
- ESP mounted to `/efi`
- ROOT fs `ext4` or `btrfs`
- Swap (partition or swapfile) of 8G
- KMS

[partitioning](#partitioning) • [formatting](#formatting) • [chroot](#chroot) • [post](#post-install) • [graphics](#graphics)

### Prerequisites

In UEFI firmware:

- ⚠ set the hardware clock to UTC time
- disable secure boot

#### Windows dual boot

Windows by default uses (and set) HW in local time \
→ force UTC, in an admin console run

```
reg add "HKEY_LOCAL_MACHINE\System\CurrentControlSet\Control\TimeZoneInformation" /v RealTimeIsUniversal /d 1 /t REG_DWORD /f
```

Re-check if HW is correctly set in UTC

https://wiki.archlinux.org/title/System_time#UTC_in_Microsoft_Windows

Disable Windows _Fast startup_

```
set reg [HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Session Manager\Power] "HiberbootEnabled" -> 0
```

https://wiki.archlinux.org/title/Dual_boot_with_Windows#Fast_Startup_and_hibernation

### Partitioning

Partition the disk using `fdisk /dev/nvme0n1`

1. create a new GPT table
2. create 1st partition of 512M (use 1G for dual boot) → ESP
3. change ESP type to "EFI system"

#### for SWAP partition

4. create 2nd partition of -8G (= remaining size for swap) → ROOT
5. ROOT type should be set by default to "Linux filesystem"
6. create 3rd part with the remaining space (8G) → SWAP
7. change SWAP type to "Linux swap"

#### if using swapfile

4. create 2nd partition of remaining size → ROOT
5. ROOT type should be set by default to "Linux filesystem"

Write partition table to disk & exit fdisk

→ https://wiki.archlinux.org/title/Partitioning

### Formatting

1. format ESP in `FAT32`

```
mkfs.fat -n ESP -F 32 /dev/nvme0n1p1
```

2. format ROOT

#### `ext4`

```
mkfs.ext4 -L ARCH /dev/nvme0n1p2
```

#### `btrfs`

```
mkfs.btrfs -L ARCH -n 32k /dev/nvme0n1p2
```

3. for SWAP partition (skip if using swapfile)

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

If `btrfs` mount with

```
mount -o compress=zstd /dev/nvme0n1p2 /mnt
```

2. mount ESP to `/efi`

```
mount --mkdir /dev/nvme0n1p1 /mnt/efi
```

### continue with Arch wiki steps

- mirrors check
- pacstrap
- fstab

https://wiki.archlinux.org/title/Installation_guide#Installation

#### Fstab & `btrfs`

After generating `fstab` edit the root entry:
remove `discard=async` and add `nodiscard` to the mount options \
→ periodic trim will be used instead

## chroot

### initramfs

Disable `fallback` img generation (useless)

- remove `fallback` from `PRESETS=(…)` line in all `.preset` files in `/etc/mkinitcpio.d/`
- remove `/boot/*-fallback.img`

Re-generate initramfs → `mkinitcpio -P`

→ https://wiki.archlinux.org/title/Mkinitcpio#Possibly_missing_firmware_for_module_XXXX

### bootloader

Install `systemd-boot` with

```
bootctl install
```

write the loader config in `/efi/loader/loader.conf`

```
default arch.conf
timeout 4
console-mode keep # or max
editor no
```

→ https://man.archlinux.org/man/loader.conf.5#OPTIONS

write the Arch Linux entry in `/efi/loader/entries/arch.conf`

```
title   Arch Linux
linux   /vmlinuz-linux
initrd  /initramfs-linux.img
options root=LABEL=ARCH rw quiet splash
```

#### copy boot file to ESP

```
cp -a /boot/vmlinuz-linux /efi/
cp -a /boot/initramfs-linux.img /efi/
```

⚠ be sure to read [system maintenance](#-system-maintenance)

### swapfile (as btrfs subvolume)

skip if using partition

```shell
btrfs subvolume create /swap
btrfs filesystem mkswapfile --size 8g --uuid clear /swap/swapfile
swapon /swap/swapfile
```

edit `/etc/fstab`

```
/swap/swapfile none swap defaults 0 0
```

→ https://wiki.archlinux.org/title/Btrfs#Swap_file

## POST install

Configure Pacman → https://github.com/doums/dot/blob/master/pacman/README.md

### network

start/enable

- `systemd-networkd.service`
- `systemd-resolved.service`

run `networkctl` to get links status

→ https://wiki.archlinux.org/title/Network_configuration \
→ https://wiki.archlinux.org/title/Systemd-networkd

#### wired setup

`/etc/systemd/network/20-wired.network`

```
[Match]
Name=eno1  # replace with the right link name

[Network]
DHCP=yes
```

#### wireless setup

`/etc/systemd/network/25-wireless.network`

```
[Match]
Name=wlan0 # replace with the right link name

[Network]
DHCP=yes
IgnoreCarrierLoss=3s
```

install `iwd` → `iwctl`

#### DNS

```
ln -sf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
sudo systemctl restart systemd-resolved.service
```

Copy `conf/dns.conf` from this repo into `/etc/systemd/resolved.conf.d/`

→ `resolvectl status`

Check DNS resolution

```shell
resolvectl query archlinux.org --cache=false
```

- https://wiki.archlinux.org/title/Systemd-resolved
- https://developers.cloudflare.com/1.1.1.1/setup/linux

#### define hosts

`/etc/hosts`

```
127.0.0.1   localhost
::1         localhost
```

#### wait-online

When multiple network interfaces are present and only one is connected,
`systemd-networkd-wait-online.service` will timeout after 2 minutes
waiting for all interfaces to be up. \
To override this, use a _drop-in_ file. Copy `systemd_unit/wait-online.conf`
into `/etc/systemd/system/systemd-networkd-wait-online.service.d/`
(create the directory) \
Then `systemctl daemon-reload`

source: https://wiki.archlinux.org/title/Systemd-networkd#Multiple_interfaces_that_are_not_connected_all_the_time

### ⚠ system maintenance

To upgrade `systemd-boot` on package upgrade use a pacman hook

To automatically copy kernel, initramfs images from
`/boot` into the ESP (`/efi`), after a system upgrade use a pacman
hook

→ https://github.com/doums/dotfiles/tree/master/pacman

### time & sync

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

### user

#### change root shell

```
chsh -s /usr/bin/fish
```

#### create default user

```
useradd -Um -G wheel -s /usr/bin/fish pierre
passwd pierre
```

→ https://wiki.archlinux.org/title/Users_and_groups#User_management

Setup sudo

### TRIM

→ https://wiki.archlinux.org/index.php/Solid_state_drive#Periodic_TRIM

```
# systemctl enable fstrim.timer
# systemctl start fstrim.timer
```

## graphics

Install the needed packages

|        | Intel        | NVIDIA             |
| ------ | ------------ | ------------------ |
| driver | mesa         | nvidia-open        |
| 32bits | lib32-mesa   | lib32-nvidia-utils |
| libs   | vulkan-intel | nvidia-utils       |

### KMS

Edit `/etc/mkinitcpio.conf`

#### Intel

```
MODULES=(i915)
```

https://wiki.archlinux.org/title/Intel_graphics#Early_KMS

#### NVIDIA

```
MODULES=(nvidia nvidia_modeset nvidia_uvm nvidia_drm)
```

⚠ remove `kms` from hooks array

> Remove kms from the HOOKS array in /etc/mkinitcpio.conf and regenerate the initramfs. This will prevent the initramfs from containing the nouveau module making sure the kernel cannot load it during early boot.

```
HOOKS=(… ~~kms~~ …)
```

Re-generate initramfs → `mkinitcpio -P`

#### NVIDIA DRM

Add `nvidia_drm.modeset=1` to kernel parameters

Edit `/efi/loader/entries/arch.conf`

```
options root=LABEL=ARCH rw quiet splash nvidia_drm.modeset=1
```

**TBD** consider using `nvidia_drm.fbdev=1`

→ https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start \
→ https://wiki.archlinux.org/title/NVIDIA#Early_loading \
→ https://wiki.archlinux.org/title/NVIDIA#DRM_kernel_mode_setting
