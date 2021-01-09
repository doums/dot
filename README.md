# My personal dotfiles


## Design porn

### gtk theme, Adapta
https://github.com/adapta-project/adapta-gtk-theme.git

(see notes below to fix HiDPI scaling problem)

### icon them
https://github.com/snwh/paper-icon-theme

resources: https://wiki.archlinux.org/index.php/Icons#Manually

## notes

The following notes are relevant to the installation of ArchLinux.

### emoji support
```
$ sudo pacman -S noto-fonts-emoji
```

### linux console font
```
$ sudo pacman -S terminus-font
```
add in `/etc/vconsole.conf`
```
FONT=ter-v22b
FONT_MAP=cp437
```

source: https://wiki.archlinux.org/index.php/HiDPI#Linux_console

### solve icon problem for apps installed through Flatpak
cd `/var/lib/flatpak/exports/share/applications`
rename the links to match this format: obs.desktop, vlc.desktop

### journal
To prevent the journal to take 4Gb space of disk memory in `/etc/systemd/journald.conf` add :
```
SystemMaxUse=1G
```
(the default is 4Gb)

source: https://wiki.archlinux.org/index.php/Systemd/Journal#Journal_size_limit

### fix Adapta theme for HiDPI
in `wm/asset/assets-xfwm-scripts/render-assets-xfwm.sh` make these changes
```
-Dide.ui.scale=2.0
Exec=env GDK_SCALE=2 steam
-    non_scale_dpi=90
+    non_scale_dpi=135
 else
-    non_scale_dpi=96
+    non_scale_dpi=144
```
then build and install:
```
./autogen.sh --prefix=/usr --disable-cinnamon --disable-mate --disable-gnome --disable-flashback --disable-openbox --enable-parallel
make
sudo make install
```
