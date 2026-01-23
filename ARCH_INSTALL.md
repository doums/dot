## Arch install

setup overview:

- UEFI - GPT
- bootloader: systemd-boot
- initramfs: mkinitcpio
- ESP mounted to `/efi`
- ROOT fs `ext4` or `btrfs`
- Swap (partition or swapfile) of 8G

[partitioning](#partitioning) • [formatting](#formatting) • [chroot](#chroot) • [post](#post-install) • [graphics](#graphics)

### Prerequisites

In UEFI firmware:

- ⚠ set the hardware clock to UTC time
- disable secure boot

### Windows dual boot

Disable Windows _Fast startup_ and _hibernation_

```
powercfg /H off
```

Download and apply the registry entries located in `win/reg/`:

- force Windows to use UTC instead of local time for HW clock
- disable _fast boot_
- disable _hibernation_

https://wiki.archlinux.org/title/Dual_boot_with_Windows#Fast_Startup_and_hibernation
https://wiki.archlinux.org/title/System_time#UTC_in_Microsoft_Windows

> [!IMPORTANT]
> By default, windows create a 100MB **EFI** partition which is too
> small.\
> The EFI needs to be manually created during windows install\
> setup using `diskpart`.
> See [instructions](win/notes.md#windows-efi)

### Partitioning

Partition the disk using `fdisk /dev/nvme0n1`

1. create a new GPT table
2. create 1st partition of 512M (use 1G for dual boot) → ESP
3. change ESP type to "EFI system"

> [!IMPORTANT]
> if dual boot skip EFI creation, the one from Windows is reused

#### for SWAP partition

4. create 2nd partition of -8G (= remaining size for swap) → ROOT
5. ROOT type should be set by default to "Linux filesystem"
6. create 3rd part with the remaining space (8G) → SWAP
7. change SWAP type to "Linux swap"

#### if using swapfile

4. create 2nd partition of remaining size → ROOT
5. ROOT type should be set by default to "Linux filesystem"

Write partition table to disk & exit fdisk

https://wiki.archlinux.org/title/Partitioning

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

https://wiki.archlinux.org/title/EFI_system_partition \
https://wiki.archlinux.org/title/File_systems

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

#### Fstab & btrfs

After generating `fstab` edit the root partition:

`discard=async` is the default for btrfs. In order to avoid\
duplicate trim operations with fstrim add `X-fstrim.notrim`\
to the btrfs partition mount options.\
This will exclude it from fstrim.

> [!NOTE]
> only relevant if fstrim periodic trim is enabled, eg. needed \
> for the other partitions.

https://wiki.archlinux.org/title/Btrfs#SSD_TRIM \
man fstrim

## chroot

### initramfs

[`mkinitcpio`](https://wiki.archlinux.org/title/Mkinitcpio) is
used to generate the initramfs (Arch's default).

In order to have the initramfs image generated in `/efi` (instead\
of the default `/boot`), the mkinitcpio preset must be updated.

1. edit `/etc/mkinitcpio.d/linux.preset` and replace the content\
   with the following:

```
#ALL_config="/etc/mkinitcpio.conf"
ALL_kver="/efi/EFI/arch/vmlinuz-linux"
PRESETS=('default')
#default_config="/etc/mkinitcpio.conf"
default_image="/efi/EFI/arch/initramfs-linux.img"
```

2. copy the kernel image from `/boot` to `/efi`

```
cp -a /boot/vmlinuz-linux /efi/EFI/arch/
```

3. re-generate initramfs → `mkinitcpio -P`

> [!TIP]
> this config also drops the linux-fallback image (useless)

> [!NOTE]
> on next updates `mkinitcpio` will copy the kernel from `/usr/lib/modules/*-arch1-1/vmlinuz`\
> into the ESP dir as `vmlinuz-linux`

> [!NOTE]
> CPU microcode (intel-ucode, amd-ucode) is packed into the initramfs\
> and picked up from `/usr/lib/firmware/*-ucode/`

https://wiki.archlinux.org/title/EFI_system_partition#Using_mkinitcpio_preset \
https://wiki.archlinux.org/title/Microcode#mkinitcpio

### bootloader

> [!IMPORTANT]
> `bootctl` will fail to update UEFI variables and boot entries
> when running in classic chroot env.\
> To create the boot entry, chroot in systemd mode using the `-S` flag

```
arch-chroot -S /mnt
```

Then install `systemd-boot` with

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

https://man.archlinux.org/man/loader.conf.5#OPTIONS

write the Arch Linux entry in `/efi/loader/entries/arch.conf`

```
title   Arch Linux
linux   /EFI/arch/vmlinuz-linux
initrd  /EFI/arch/initramfs-linux.img
options root=LABEL=ARCH rw quiet splash
```

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

https://wiki.archlinux.org/title/Btrfs#Swap_file

## POST install

Configure Pacman → https://github.com/doums/dot/blob/master/pacman/README.md

### network

start/enable

- `systemd-networkd.service`
- `systemd-resolved.service`

run `networkctl` to get links status

https://wiki.archlinux.org/title/Network_configuration \
https://wiki.archlinux.org/title/Systemd-networkd

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

#### hosts file

It is a good practice to add an entry for the local hostname

`/etc/hosts`

```
127.0.0.1   localhost
::1         localhost
# new entry
127.0.1.1   <hostname>
```

https://wiki.archlinux.org/title/Network_configuration#local_hostname_is_resolved_over_the_network

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

Some pacman hooks are needed to

- upgrade `systemd-boot` on system upgrade
- clear pacman packages cache

see https://github.com/doums/dotfiles/tree/master/pacman

### time & sync

https://wiki.archlinux.org/title/System_time \
https://wiki.archlinux.org/title/Systemd-timesyncd

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

https://wiki.archlinux.org/title/Users_and_groups#User_management

Setup sudo

### TRIM

Enable periodic TRIM

https://wiki.archlinux.org/title/Solid_state_drive#Periodic_TRIM

```
# systemctl enable fstrim.timer
# systemctl start fstrim.timer
```

To check which partitions will be trimmed

```
# journalctl -u fstrim.service
# systemctl cat fstrim.service
# this is the command used by the timer, in dry-run mode
sudo fstrim --listed-in /etc/fstab:/proc/self/mountinfo --verbose --dry-run
```

## graphics

Install the needed packages

|        | Intel        | NVIDIA             |
| ------ | ------------ | ------------------ |
| driver | mesa         | nvidia-open        |
| 32bits | lib32-mesa   | lib32-nvidia-utils |
| libs   | vulkan-intel | nvidia-utils       |

> [!TIP]
> Intel: to run the new `xe` driver see\
> https://wiki.archlinux.org/title/Intel_graphics#Testing_the_new_experimental_Xe_driver

### Early KMS

> [!NOTE]
> Early KMS should not be set when using **hibernation**.\
> See
> https://wiki.archlinux.org/title/Power_management/Suspend_and_hibernate#Suspend/hibernate_does_not_work,_or_does_not_work_consistently\

#### Intel

Should be enabled by default as the `kms` hook is present in the
initramfs `/etc/mkinitcpio.conf`:

```
HOOKS=(… ~~kms~~ …)
```

https://wiki.archlinux.org/title/Intel_graphics#Early_KMS

#### NVIDIA

Late KMS should be enabled by default, to check run

```shell
cat /sys/module/nvidia_drm/parameters/modeset
```

`Y` -> enabled

https://wiki.archlinux.org/title/NVIDIA#DRM_kernel_mode_setting

To enable kms at initramfs stage add the following modules in `/etc/mkinitcpio.conf`:

```
MODULES=(nvidia nvidia_modeset nvidia_uvm nvidia_drm)
```

⚠ Also remove `kms` from hooks array

> Remove kms from the HOOKS array in /etc/mkinitcpio.conf and
regenerate the initramfs.\
This will prevent the initramfs from containing the nouveau module\
making sure the kernel cannot load it during early boot.

```
HOOKS=(… ~~kms~~ …)
```

Re-generate initramfs → `mkinitcpio -P`

https://wiki.archlinux.org/title/Kernel_mode_setting#Early_KMS_start \
https://wiki.archlinux.org/title/NVIDIA#Early_loading \
https://wiki.archlinux.org/title/NVIDIA#DRM_kernel_mode_setting

### NVIDIA suspend and hibernation

Check the following services are enabled, should be by default:

```
nvidia-suspend.service
nvidia-resume.service
nvidia-suspend-then-hibernate.service
```

If not, enable them.

#### Restore video memory after suspend

Enabled by default, to check:

```shell
sudo sort /proc/driver/nvidia/params | rg 'PreserveVideoMemoryAllocations|TemporaryFilePath'

# output:
# PreserveVideoMemoryAllocations: 1
# TemporaryFilePath: "/var/tmp"
```

> [!NOTE]
> Early KMS should not be set if hibernation is used

> when the loading of nvidia module happens in the initramfs, it has no access to NVreg_TemporaryFilePath which stores the previous video memory: early KMS should not be used if hibernation is desired.

https://wiki.archlinux.org/title/NVIDIA/Tips_and_tricks#Preserve_video_memory_after_suspend

