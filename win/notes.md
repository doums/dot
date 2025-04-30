### Windows EFI

By default, Windows will create a 100MB EFI system part which is
too small.\
Arch's [ref](https://wiki.archlinux.org/title/Dual_boot_with_Windows#The_EFI_system_partition_created_by_Windows_Setup_is_too_small)
suggests creating the EFI from Arch live media,\
before Windows install, to set a bigger size. Then Windows would reuse it.\
Attractive solution but it's not.

For some reason this failed when I tried it (04/2025):

- windows bootloader was half broken: blue recovery screen at boot\
  and need to select manually the proper boot volume
- during Arch install the partition was actually sized as 100MB\
  even if reported with the initial 1G size, so initramfs gen failed\
  because of "not enough space"

#### working solution

Create the EFI partition manually during Windows install Setup,\
using `diskpart.exe` (Shift+F10)

1. select the disk, ⚠ it should have no partitions; and apply,\
   windows will create a bunch of partitions
2. remove them, except the `Recovery` one if present (it was not),\
   so I removed them all
3. `Shift+F10` → launch `diskpart.exe`, create the EFI (size in
   MiB)

```
list disk
select disk X
create partition efi size=1024
format quick fs=fat32 label=System
```

4. exit diskpart and cmd prompt to go back to the GUI
5. create the main partition for windows with the desired size (in MiB)
6. select it and continue the install

https://www.ctrl.blog/entry/how-to-esp-windows-setup.html
https://wiki.archlinux.org/title/Dual_boot_with_Windows#The_EFI_system_partition_created_by_Windows_Setup_is_too_small
https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/diskpart

> [!TIP]
> Later on Arch rename the EFI with `fatlabel /dev/XXX "LABEL"` [ref](https://wiki.archlinux.org/title/Persistent_block_device_naming#by-label)
