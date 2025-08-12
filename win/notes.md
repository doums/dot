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

Create the EFI partition manually during Windows install Setup\
At _Select disk_ step:

1. `Shift+F10` → run `diskpart.exe`, create the EFI (size in MiB)

```
list disk
select disk X
create partition efi size=1024
format quick fs=fat32 label=System
```

2. exit `diskpart`, in the GUI click _Refresh_, the freshly created\
   EFI partition should show up
3. create the main partition for windows with the desired size

> [!IMPORTANT]
> Size unit is MiB (mebibyte), eg. 204800 for 200GB\
> Use a converter GiB to MiB to get the desired MiB value

4. select it and continue the install

https://www.ctrl.blog/entry/how-to-esp-windows-setup.html
https://wiki.archlinux.org/title/Dual_boot_with_Windows#The_EFI_system_partition_created_by_Windows_Setup_is_too_small
https://learn.microsoft.com/en-us/windows-server/administration/windows-commands/diskpart

> [!TIP]
> Later on Arch rename the EFI with `fatlabel /dev/XXX "LABEL"` [ref](https://wiki.archlinux.org/title/Persistent_block_device_naming#by-label)
