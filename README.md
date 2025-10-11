# dot

My dotfiles

## OS installation

Arch Linux build:

- display server: Xorg (X11)
- no desktop environment
- display manager: LightDM
- window manager: XMonad
- compositor: yshui/picom

[INSTALL](ARCH_INSTALL.md)

## X

Install `xorg-server`

https://wiki.archlinux.org/title/Xorg

#### compositor

Install `picom`

## Audio

Install the following packages

```
pipewire wireplumber pipewire-pulse pipewire-alsa pavucontrol
```

## Display manager

Install LightDM and its GTK greeter

```
lightdm lightdm-gtk-greeter
```

Enable `lightdm.service`

#### config

Copy `lightdm/lightdm-gtk-greeter.conf` to `/etc/lightdm/`

https://wiki.archlinux.org/title/LightDM#Installation

## Window Manager

### fonts

Main fonts

```
ttf-inconsolata ttf-roboto noto-fonts ttf-dejavu ttf-liberation
```

Emoji font

```
noto-fonts-emoji
```

Refresh font cache

```shell
fc-cache
fc-list
```

#### custom fonts

[docs](https://wiki.archlinux.org/title/Fonts#Manual_installation)

First create the directory `/usr/local/share/fonts/ttf/` and
place any custom fonts under it

---

### XMonad

> [!IMPORTANT]
> Most of the required system deps should be already installed (via xorg).\
> But some of them could be missing and need to be installed,\
> like `libxss` [ref](https://xmonad.org/INSTALL.html#arch)

#### stack

Then install `stack`. Take the bin version from AUR to avoid the
plethora of Haskell dependencies.

```shell
rua install stack-bin
```

#### sources

Clone the sources in `/opt/xmonad`

```shell
git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib
```

Copy the config file `xmonad/xmonad.hs` to `/opt/xmonad/`.\
Copy the `xmonad/lib` directory to `/opt/xmonad/` (it contains a
custom layout).\
Init the project to generate the `stack.yaml` file

```shell
stack init
```

#### build & install

```shell
stack install
```

Xmonad expects `xmonad.hs` and `stack.yaml` to be in
`~/.config/xmonad/` \
Use symlinks instead (does not make sense to have source files
under the config directory)

```shell
cd ~/.config/xmonad
ln -s /opt/xmonad/xmonad.hs xmonad.hs
ln -s /opt/xmonad/stack.yaml stack.yaml
```

source: https://xmonad.org/INSTALL.html

#### init with lightDM

Copy `xsessions/xmonad.desktop` in `/usr/share/xsessions`

source: https://wiki.archlinux.org/title/Display_manager#Session_configuration

#### XMobar

Clone the sources and build the project

```shell
git clone https://codeberg.org/xmobar/xmobar.git
```

Override `stack.yaml` to only build needed features

```shell
cp xmobar/stack.yaml .
```

Add `xmobarrc` to `~/config/xmobar/`.

Finally build and install it

```shell
stack install
```

##### required packages

```
rofi dmenu clipmenu trayer shotgun graphicsmagick xwallpaper
udiskie xclip slop gpick ouch
```

aur

```
trashy
```

### XDG user directories

```shell
sudo pacman -S xdg-user-dirs
xdg-user-dirs-update
```

source: https://wiki.archlinux.org/title/XDG_user_directories

> [!TIP]
> To remove unwanted dirs like `~/Desktop`, `~/Templates` or `~/Public`\
> just `rm -rf` them then run `xdg-user-dirs-update`

## Screen lock

Install `i3lock-color` from AUR

#### On suspend

Copy `systemd_unit/suspend@.service` to `/etc/systemd/system/`
and enable it

```
sudo systemctl enable suspend@pierre.service
```

https://wiki.archlinux.org/title/Power_management/Suspend_and_hibernate#Sleep_hooks

## UI style

### Theme

Install [materia](https://github.com/nana-4/materia-theme)
`materia-gtk-theme`

edit `.config/gtk-3.0/settings.ini`

```
[Settings]
gtk-icon-theme-name = Papirus
gtk-theme-name = Materia-dark
gtk-font-name = Roboto 12
```

edit ~/.gtkrc-2.0

```
gtk-icon-theme-name = "Papirus"
gtk-theme-name = "Materia-dark"
gtk-font-name = "Roboto 12"
```

For gnome apps

```shell
gsettings set org.gnome.desktop.interface gtk-theme Materia-dark
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
gsettings set org.gnome.desktop.interface icon-theme Papirus
gsettings set org.gnome.desktop.interface cursor-theme Paper
gsettings set org.gnome.desktop.interface cursor-size 48
```

#### Flatpak

```shell
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.gtk.Gtk3theme.Materia-dark
```

### Icon theme

Install `hicolor-icon-theme` and `papirus-icon-theme` [Papirus](https://github.com/PapirusDevelopmentTeam/papirus-icon-theme)

Edit `/usr/share/icons/default/index.theme`

```
[Icon Theme]
Inherits=Papirus
```

https://wiki.archlinux.org/title/Icons#Manually

### Cursor theme

Install https://github.com/snwh/paper-icon-theme

> [!IMPORTANT]
> install under `/usr` (cmake prefix) as it must be accessible\
> by lightDM which supports only `/usr/share/icons` path

To apply the theme edit `.Xresources`

```
Xcursor.theme: Paper
```

https://wiki.archlinux.org/title/Cursor_themes

### set the wallpaper

Install `xwallpaper` package\
`xwallpaper` is spawned by XMonad\
The wallpaper image is defined by `$BG_PRIMARY` env variable\
eg. in `.xprofile`

```bash
export BG_PRIMARY=$HOME/Pictures/dark_ocean.png
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

- https://wiki.archlinux.org/title/HiDPI#X_Resources
- https://wiki.archlinux.org/title/LightDM#Environment_variables

larger font for linux console

```shell
$ sudo pacman -S terminus-font
```

add in `/etc/vconsole.conf`

```
FONT=ter-v22b
FONT_MAP=cp437
```

source: https://wiki.archlinux.org/title/HiDPI#Linux_console_(tty)

## notes

The following notes are relevant to the installation of ArchLinux.

### solve icon problem for apps installed through Flatpak

```shell
cd /var/lib/flatpak/exports/share/applications
```

rename the links to match this format: obs.desktop, vlc.desktop

### journal

To prevent the journal to take 4Gb space of disk memory copy the
configuration file `00-journal-size.conf` (from the `conf` dir in
this repo) into `/etc/systemd/journald.conf.d/` directory (create it)

(the default is 4Gb)

source: https://wiki.archlinux.org/title/Systemd/Journal#Journal_size_limit

### SSH agent

Install `gnome-keyring` and `libsecret`

Enable the following systemd **user** unit (⚠ no `sudo`)

```shell
systemctl --user enable gcr-ssh-agent.socket
```

Set the env var `SSH_AUTH_SOCK` to `/run/user/1000/gcr/ssh`
(`.xprofile`/ fish config)

→ https://wiki.archlinux.org/title/GNOME/Keyring

### Firefox

`about:config` tweaks:

- increase UI size

```
ui.textScaleFactor 180
```

- scrollbar width

```
widget.non-native-theme.scrollbar.size.override 48
widget.non-native-theme.enable true
```

### Bluetooth

install

```
bluez bluetui
```

start/enable `bluetooth.service`

### TrackPoint

To customize the speed use the _magic trackpoint multiplier_\
First, find the device name

```
sudo libinput list-devices | rg -i trackpoint
```

Create `/etc/libinput/local-overrides.quirks`

```
[Trackpoint Override]
MatchUdevType=pointingstick
MatchName=*TPPS/2 Elan TrackPoint*
AttrTrackpointMultiplier=1.5
```

Default multiplier value is `1.0`

ref:\
https://wayland.freedesktop.org/libinput/doc/latest/trackpoint-configuration.html\
ref https://wiki.archlinux.org/title/TrackPoint#device-quirks
