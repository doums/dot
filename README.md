# dot

My dotfiles

## OS installation

Arch Linux build:

- display server: Xorg (X11)
- no desktop environment
- display manager: LightDM + its GTK greeter
- window manager: XMonad
- compositor: yshui/picom

[INSTALL](ARCH_INSTALL.md)

## X

Install `xorg-server`

https://wiki.archlinux.org/index.php/Xorg

## Display manager

Install LightDM and its GTK greeter

```
lightdm lightdm-gtk-greeter
```

Enable `lightdm.service`

https://wiki.archlinux.org/index.php/LightDM#Installation

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

### XMonad

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
dmenu clipmenu trayer shotgun graphicsmagick xwallpaper udisks2 
udiskie xclip slop gpick trashy ouch
```

### XDG user directories

```shell
sudo pacman -S xdg-user-dirs
xdg-user-dirs-update
```

source: https://wiki.archlinux.org/index.php/XDG_user_directories

## Screen lock

Install `i3lock-color` from AUR

#### On suspend

Copy `systemd_unit/suspend@.service` to `/etc/systemd/system/`
and enable it

```
sudo systemctl enable suspend@pierre.service
```

https://wiki.archlinux.org/title/Power_management/Suspend_and_hibernate#Sleep_hooks

## Design

### GTK theme

Theme used https://github.com/nana-4/materia-theme

Install `materia-gtk-theme`

edit `$XDG_CONFIG_HOME/gtk-3.0/settings.ini`

```
[Settings]
gtk-icon-theme-name = Paper
gtk-theme-name = Materia-dark
gtk-font-name = Roboto 12
```

edit ~/.gtkrc-2.0

```
gtk-icon-theme-name = "Paper"
gtk-theme-name = "Materia-dark"
gtk-font-name = "Roboto 12"
```

#### gnome

```shell
gsettings set org.gnome.desktop.interface gtk-theme Materia-dark
gsettings set org.gnome.desktop.interface color-scheme prefer-dark
gsettings set org.gnome.desktop.interface icon-theme Paper
gsettings set org.gnome.desktop.interface cursor-size 64
```

#### Flatpak

```shell
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install flathub org.gtk.Gtk3theme.Materia-dark
```

### Cursor theme

install https://github.com/snwh/paper-icon-theme

edit `/usr/share/icons/default/index.theme`

```
[Icon Theme]
Inherits=Paper
```

note: the icon theme is also set when `.Xresources` file is read
when lightdm (display manager) starts and sources `.xprofile`

- https://wiki.archlinux.org/title/Cursor_themes
- https://wiki.archlinux.org/index.php/Icons#Manually

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

To prevent the journal to take 4Gb space of disk memory copy the
configuration file `00-journal-size.conf` (from the `conf` dir in
this repo) into `/etc/systemd/journald.conf.d/` directory (create it)

(the default is 4Gb)

source: https://wiki.archlinux.org/index.php/Systemd/Journal#Journal_size_limit

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

To set the scrollbar width set the following property in
`about:config`

```
widget.non-native-theme.scrollbar.size.override 16
```

NOTE: the `widget.non-native-theme.enable` should be set to true
