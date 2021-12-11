# My personal dotfiles

## ArchLinux installation

Linux build:

- display server: Xorg (X11)
- no desktop environment
- display manager: LightDM + its gtk greeter
- window manager: XMonad
- compositor: picom

### in the live environment

Connect in wireless using `iwd`

```shell
$ iwctl
```

resources: https://wiki.archlinux.org/index.php/Iwd#Connect_to_a_network

### during the arch-chroot session (aka Inception)

Install `base-devel` and `networkmanager` packages

```shell
pacman -S base-devel networkmanager
```

### first boot in the fresh installed Linux

start the service `NetworkManager.service`

```shell
sudo systemctl enable NetworkManager.service
sudo systemctl start NetworkManager.service
```

then connect to a network using `nmtui`

source: https://wiki.archlinux.org/index.php/NetworkManager#Installation

### enable periodic TRIM

```shell
sudo systemctl enable fstrim.timer
sudo systemctl start fstrim.timer
```

source: https://wiki.archlinux.org/index.php/Solid_state_drive#Periodic_TRIM

### create XDG user directories

```shell
sudo pacman -S xdg-user-dirs
xdg-user-dirs-update
```

source: https://wiki.archlinux.org/index.php/XDG_user_directories

### graphical environment setup

1. install the display server Xorg (implementation of the X Window System aka X11) and the display driver (`mesa` for intel integrated graphic card)

```shell
sudo pacman -S xorg-server mesa
```

https://wiki.archlinux.org/index.php/Xorg\
https://wiki.archlinux.org/index.php/Intel_graphics#Installation

2. install the display manager LightDM and its greeter

```shell
sudo pacman -S lightdm lightdm-gtk-greeter
```

start the service

```shell
sudo systemctl enable lightdm
```

https://wiki.archlinux.org/index.php/LightDM#Installation

3. install the window manager

#### XMonad

First install `stack`. Take the static version from AUR to avoid
the plethora of Haskell dependencies.

```shell
rua install stack-static
```

Chose a location to clone the sources and build the project

```shell
mkdir Documents/xmonad
cd Documents/xmonad
git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib
```

Add the configuration file from `xmonad/xmonad.hs` and init the
project to generate the `stack.yaml` file

```shell
stack init
```

Then build and install it

```shell
stack install
```

This will install `xmonad` binary under `~/.local/bin`.

`xmonad --recompile` expects to find the project configuration and
the XMonad config in `$XDG_CONFIG_HOME/xmonad`. To avoid to move
the whole project under `$XDG_CONFIG_HOME/xmonad` (which doesn't
make sense) symlink these files.

```shell
mkdir ~/.config/xmonad
cd ~/.config/xmonad
ln -s ~/Documents/xmonad/xmonad.hs xmonad.hs
ln -s ~/Documents/xmonad/stack.yaml stack.yaml
```

source: https://xmonad.org/INSTALL.html

##### XMobar

Clone the sources and build the project

```shell
git clone git://github.com/jaor/xmobar
```

Override `stack.yaml` to only build needed features

```shell
cp xmobar/stack.yaml .
```

Add `xmobarrc_primary` and `xmobarrc_secondary` to `~/config/xmobar/`.

Finally build and install it

```shell
stack install
```

source:https://xmobar.org/#from-source

##### system tray

Install trayer

```shell
sudo pacman -S trayer
```

### mount helper and USB

```shell
sudo pacman -S udisks2 udiskie
```

source: https://wiki.archlinux.org/index.php/Udisks

## Design porn

### gtk theme, Adapta

https://github.com/adapta-project/adapta-gtk-theme.git

(see notes below for how to build it for HiDPI)

### icon and cursor theme

install https://github.com/snwh/paper-icon-theme

edit `/usr/share/icons/default/index.theme`

```
[Icon Theme]
Inherits=Paper
```

note: the icon theme is also set when `.Xresources` file is read when lightdm (display manager) starts and sources `.xprofile`

resources:

- https://wiki.archlinux.org/index.php/Icons#Manually
- https://wiki.archlinux.org/index.php/Cursor_themes#XDG_specification

### set gtk theme

in `$XDG_CONFIG_HOME/gtk-3.0/settings.ini`

```
[Settings]
gtk-icon-theme-name = Paper
gtk-theme-name = Adapta-Nokto
gtk-font-name = Roboto 12
```

in ~/.gtkrc-2.0

```
gtk-icon-theme-name = "Paper"
gtk-theme-name = "Adapta-Nokto"
gtk-font-name = "Roboto 12"
```

### fonts

first install some TTF fonts

```shell
sudo pacman -S noto-fonts ttf-dejavu ttf-liberation
```

put the files located in the `font` directory in `/usr/share/fonts/TTF` and make them readable by every user

update de font cache

```shell
fc-cache
```

**emoji support**

```shell
$ sudo pacman -S noto-fonts-emoji
```

**patch font to add MDI icons glyphs**

Material Design Icons font: https://github.com/Templarian/MaterialDesign-Font

font patcher: https://github.com/ryanoasis/nerd-fonts#font-patcher

1. install FontForge

```shell
sudo pacman -S FontForge
```

2. clone the repo

```shell
git clone --depth 1 https://github.com/ryanoasis/nerd-fonts.git
cd nerd-fonts
```

3. move the MDI font in `src/glyphs/`
4. patch

```shell
./font-patcher --custom MaterialDesignIconsDesktop.ttf --progressbars --careful path/to/fontToPatch.ttf
```

### HiDPI

DPI is set in `.Xresources`

```
Xft.dpi: 144
```

it will be loaded during display manager (ligthdm) starts and sources `.xprofile`

```sh
[[ -f "$home/.Xresources" ]] && xrdb -merge $home/.Xresources
```

sources:

- https://wiki.archlinux.org/index.php/HiDPI#X_Resources
- https://wiki.archlinux.org/index.php/LightDM#Environment_variables

larger font for linux console

```shell
$ sudo pacman -S terminus-font
```

add in `/etc/vconsole.conf`

```
FONT=ter-v22b
FONT_MAP=cp437
```

source: https://wiki.archlinux.org/index.php/HiDPI#Linux_console

## notes

The following notes are relevant to the installation of ArchLinux.

### solve icon problem for apps installed through Flatpak

```shell
cd /var/lib/flatpak/exports/share/applications
```

rename the links to match this format: obs.desktop, vlc.desktop

### journal

To prevent the journal to take 4Gb space of disk memory in `/etc/systemd/journald.conf` add :

```
SystemMaxUse=1G
```

(the default is 4Gb)

source: https://wiki.archlinux.org/index.php/Systemd/Journal#Journal_size_limit

### fix Adapta theme for HiDPI

install build dependencies

```shell
sudo pacman -S inkscape sassc parallel
```

in `wm/asset/assets-xfwm-scripts/render-assets-xfwm.sh` make these changes

```
 if [ "$inkver" = 0.91 ]; then
-    non_scale_dpi=90
+    non_scale_dpi=135
 else
-    non_scale_dpi=96
+    non_scale_dpi=144
 fi
```

then build and install:

```shell
./autogen.sh --prefix=/usr --disable-cinnamon --disable-mate --disable-gnome --disable-flashback --disable-openbox --enable-parallel
make
sudo make install
```

remove build dependencies

```shell
sudo pacman -Rsn inkscape sassc parallel
```

### gnome keyring

```shell
sudo pacman -S gnome-keyring
```

https://wiki.archlinux.org/index.php/GNOME/Keyring#Installation

### lower GRUB resolution

In `/etc/default/grub` change the line (replace with the desired resolutions)

```
GRUB_GFXMODE=1600x1200x32,2560x1440x32,auto
```

then re-generate the config

```shell
grub-mkconfig -o /boot/grub/grub.cfg
```

source: https://wiki.archlinux.org/index.php/GRUB/Tips_and_tricks#Setting_the_framebuffer_resolution
