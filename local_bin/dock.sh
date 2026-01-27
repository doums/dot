#!/bin/bash

set -E
set -o pipefail
DIFFS=1

# ANSI style codes
RED="\e[38;5;1m" # red
GRN="\e[38;5;2m" # green
YLW="\e[38;5;3m" # yellow
MGT="\e[38;5;5m" # magenta
BLD="\e[1m"      # bold
ITL="\e[3m"      # italic
RS="\e[0m"       # style reset
B_RED="$BLD$RED"
B_GRN="$BLD$GRN"
B_YLW="$BLD$YLW"
B_MGT="$BLD$MGT"
BI_MGT="$ITL$B_MGT"
####

trap 'catch_err $? $LINENO' ERR

# Files to update
xresources="$HOME"/.Xresources
xmonad_hs=/opt/xmonad/xmonad.hs
xmobar_cfg="$XDG_CONFIG_HOME"/xmobar/xmobarrc
rofi_cfg="$XDG_CONFIG_HOME"/rofi/config.rasi
rofi_theme="$XDG_CONFIG_HOME"/rofi/theme.rasi
wezterm_cfg="$XDG_CONFIG_HOME"/wezterm/wezterm.lua
gtk3_cfg="$XDG_CONFIG_HOME"/gtk-3.0/settings.ini
traycfg_dir="$XDG_CONFIG_HOME"/stalonetray
tray_cfg="$traycfg_dir"/config

# Safe edit infrastructure
BACKUP_DIR="$HOME/.cache/screen_setup/bak"
TMP_DIR=""

# shellcheck disable=SC2329
catch_err() {
  trap - ERR
  echo -e " $B_RED✗$RS unexpected error, [$BLD$1$RS] L#$BLD$2$RS"
  rm -rf "$TMP_DIR"
  exit 1
}

init_dirs() {
  mkdir -p "$BACKUP_DIR"
  TMP_DIR=$(mktemp -d)
}

cleanup() {
  rm -rf "$TMP_DIR"
}

log() {
  echo -e "$1"
}

prepare_file() {
  local src="$1"
  local name
  name=$(basename "$src")
  cp "$src" "$TMP_DIR/$name"
  cp "$src" "$BACKUP_DIR/$name"
  log "  ${B_GRN}backdup$RS $B_YLW$name$RS to $B_YLW$BACKUP_DIR/$name$RS"
}

apply_file() {
  local src="$1"
  local name
  name=$(basename "$src")
  if ! cmp -s "$TMP_DIR/$name" "$src"; then
    cp "$TMP_DIR/$name" "$src"
    log "  ${B_GRN}updated$RS $B_YLW$src$RS"
  fi
}

tmp_path() {
  local name
  name=$(basename "$1")
  echo "$TMP_DIR/$name"
}

# integrated display
declare -A int_cfg
int_cfg[dpi]=192
int_cfg[xcursor_size]=72
int_cfg[xmonad_borders]=6
int_cfg[xmonad_gaps]=9
int_cfg[xmonad_dfsize]=31
int_cfg[xmobar_dpi]=168
int_cfg[xmobar_pos]="52 0 232 0 0"
int_cfg[xmobar_font]="JetBrainsMono SZMDI 14"
int_cfg[traycfg]=qhd43_config
int_cfg[rofi_yoffset]=380px
int_cfg[rofi_dpi]=192
int_cfg[wez_fontsize]=13.0
int_cfg[wez_uthick]=8
# shellcheck disable=SC2034
int_cfg[gtkcursor_size]=72

# FHD BenQ XL2546X
declare -A FHD_cfg
FHD_cfg[dpi]=96
FHD_cfg[xcursor_size]=32
FHD_cfg[xmonad_borders]=4
FHD_cfg[xmonad_gaps]=5
FHD_cfg[xmonad_dfsize]=18
FHD_cfg[xmobar_dpi]=96
FHD_cfg[xmobar_pos]="32 0 148 0 0"
FHD_cfg[xmobar_font]="JetBrainsMono SZMDI 11"
FHD_cfg[traycfg]=fhd_config
FHD_cfg[rofi_yoffset]=380px
FHD_cfg[rofi_dpi]=120
FHD_cfg[wez_fontsize]=13.0
FHD_cfg[wez_uthick]=6
# shellcheck disable=SC2034
FHD_cfg[gtkcursor_size]=32

# QHD 1440p AW2725DF
declare -A AW_cfg
AW_cfg[dpi]=120
AW_cfg[xcursor_size]=48
AW_cfg[xmonad_borders]=4
AW_cfg[xmonad_gaps]=5
AW_cfg[xmonad_dfsize]=22
AW_cfg[xmobar_dpi]=120
AW_cfg[xmobar_pos]="32 0 148 0 0"
AW_cfg[xmobar_font]="JetBrainsMono SZMDI 12"
AW_cfg[traycfg]=qhd_config
AW_cfg[rofi_yoffset]=400px
AW_cfg[rofi_dpi]=120
AW_cfg[wez_fontsize]=13.0
AW_cfg[wez_uthick]=6
# shellcheck disable=SC2034
AW_cfg[gtkcursor_size]=48

update_cfg() {
  local -n cfg=$1

  init_dirs

  # prepare all files for safe editing
  prepare_file "$xresources"
  prepare_file "$xmonad_hs"
  prepare_file "$xmobar_cfg"
  prepare_file "$tray_cfg"
  prepare_file "$rofi_cfg"
  prepare_file "$rofi_theme"
  prepare_file "$wezterm_cfg"
  prepare_file "$gtk3_cfg"

  # update Xresources DPI
  sed -Ei "s|^Xft.dpi.+|Xft.dpi: ${cfg[dpi]}|" "$(tmp_path "$xresources")"
  sed -Ei "s|^Xcursor.size.+|Xcursor.size: ${cfg[xcursor_size]}|" "$(tmp_path "$xresources")"
  # update XMonad config
  sed -Ei "s|^borders =.+|borders = ${cfg[xmonad_borders]}|" "$(tmp_path "$xmonad_hs")"
  sed -Ei "s|^gaps =.+|gaps = ${cfg[xmonad_gaps]}|" "$(tmp_path "$xmonad_hs")"
  sed -Ei "s|^dmenuFnSize =.+|dmenuFnSize = ${cfg[xmonad_dfsize]}|" "$(tmp_path "$xmonad_hs")"
  # update XMobar config
  sed -Ei "s|font = \".+\"|font = \"${cfg[xmobar_font]}\"|" "$(tmp_path "$xmobar_cfg")"
  sed -Ei "s|dpi = .+|dpi = ${cfg[xmobar_dpi]}|" "$(tmp_path "$xmobar_cfg")"
  sed -Ei "s|position = BottomHM.+|position = BottomHM ${cfg[xmobar_pos]}|" "$(tmp_path "$xmobar_cfg")"
  # update rofi style
  sed -Ei "s|dpi: .+;|dpi: ${cfg[rofi_dpi]};|" "$(tmp_path "$rofi_cfg")"
  sed -Ei "s|y-offset: .+;|y-offset: ${cfg[rofi_yoffset]};|" "$(tmp_path "$rofi_theme")"
  # update wezterm config
  sed -Ei "s|^c.dpi = .+|c.dpi = ${cfg[dpi]}|" "$(tmp_path "$wezterm_cfg")"
  sed -Ei "s|^c.font_size = .+|c.font_size = ${cfg[wez_fontsize]}|" "$(tmp_path "$wezterm_cfg")"
  sed -Ei "s|^c.underline_thickness = .+|c.underline_thickness = ${cfg[wez_uthick]}|" "$(tmp_path "$wezterm_cfg")"
  # update gtk3 config
  sed -Ei "s|^gtk-cursor-theme-size = .+|gtk-cursor-theme-size = ${cfg[gtkcursor_size]}|" "$(tmp_path "$gtk3_cfg")"
  # override stalonetray config
  cp -f "$traycfg_dir/${cfg[traycfg]}" "$(tmp_path "$tray_cfg")"

  # apply all changes
  apply_file "$xresources"
  apply_file "$xmonad_hs"
  apply_file "$xmobar_cfg"
  apply_file "$tray_cfg"
  apply_file "$rofi_cfg"
  apply_file "$rofi_theme"
  apply_file "$wezterm_cfg"
  apply_file "$gtk3_cfg"

  # show diffs
  if [ $DIFFS -eq 1 ]; then
    log "${BI_MGT}--- changes applied ---${RS}"
    delta -s --paging=never "$BACKUP_DIR/.Xresources" "$xresources" || true
    delta -s --paging=never "$BACKUP_DIR/xmonad.hs" "$xmonad_hs" || true
    delta -s --paging=never "$BACKUP_DIR/xmobarrc" "$xmobar_cfg" || true
    delta -s --paging=never "$BACKUP_DIR/config" "$tray_cfg" || true
    delta -s --paging=never "$BACKUP_DIR/config.rasi" "$rofi_cfg" || true
    delta -s --paging=never "$BACKUP_DIR/theme.rasi" "$rofi_theme" || true
    delta -s --paging=never "$BACKUP_DIR/wezterm.lua" "$wezterm_cfg" || true
    delta -s --paging=never "$BACKUP_DIR/settings.ini" "$gtk3_cfg" || true
  fi

  # load new X properties
  # NOTE: running apps would need relaunch in order to see the
  #   changes
  xrdb -load ~/.Xresources
  # restart xmonad
  xmonad --recompile && xmonad --restart

  cleanup
  log
  log "${MGT}*${BI_MGT}DONE${RS}${MGT}*"
}

# switch on/off wlan0 and keyboard backlight
# $1: on | off
dock_mode() {
  if [ "$1" == 'on' ]; then
    if iwctl device wlan0 show | rg -q 'Powered\s+on'; then
      iwctl device wlan0 set-property Powered off
      echo 'wlan0 switched off'
    fi
    brightnessctl -d tpacpi::kbd_backlight set 0 &>/dev/null
  else
    if iwctl device wlan0 show | rg -q 'Powered\s+off'; then
      iwctl device wlan0 set-property Powered on
      echo 'wlan0 switched on'
    fi
    brightnessctl -d tpacpi::kbd_backlight set 1 &>/dev/null
  fi
}

setup_laptop() {
  xrandr --output DP-1 --off \
    --output DP-2 --off \
    --output DP-1-2 --off \
    --output DP-1-3 --off \
    --output eDP-1 --primary --mode 2880x1800_120

  dock_mode 'off'
}

benq_dualup() {
  xrandr --output eDP-1 --off \
    --output DP-1-2 --auto --pos 0x0 \
    --output DP-2 --primary --mode 1920x1080_240 --pos 2560x1260

  dock_mode 'on'
}

aw_dualup() {
  xrandr --output eDP-1 --off \
    --output DP-1-2 --auto --pos 0x0 \
    --output DP-2 --primary --mode 2560x1440_240 --pos 2560x1260

  dock_mode 'on'
}

fzf_status=0
choices=(
  'setup AW & dualUp'
  'setup benQ & dualUp'
  'laptop'
  'cfg AW'
  'cfg BenQ'
)
choice=$(printf "%s\n" "${choices[@]}" |
  fzf --no-info --header="setup") || fzf_status=$?

if [[ $fzf_status -eq 130 ]] || [[ -z "$choice" ]]; then
  exit 0
fi

case "$choice" in
"setup benQ & dualUp")
  benq_dualup
  ;;
"setup AW & dualUp")
  aw_dualup
  ;;
"laptop")
  update_cfg 'int_cfg'
  setup_laptop
  ;;
"cfg AW")
  update_cfg 'AW_cfg'
  ;;
"cfg BenQ")
  update_cfg 'FHD_cfg'
  ;;
*) exit 1 ;;
esac

exit 0
