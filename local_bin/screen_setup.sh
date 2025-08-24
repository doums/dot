#!/bin/bash

set -e

# ANSI style codes
GRN="\e[38;5;2m" # green
YLW="\e[38;5;3m" # yellow
MGT="\e[38;5;5m" # magenta
BLD="\e[1m"      # bold
ITL="\e[3m"      # italic
RS="\e[0m"       # style reset
B_GRN="$BLD$GRN"
B_YLW="$BLD$YLW"
B_MGT="$BLD$MGT"
BI_MGT="$ITL$B_MGT"
####

xmonad_hs=/opt/xmonad/xmonad.hs
xconfdir=/etc/X11/xorg.conf.d
xmobar_cfg="$XDG_CONFIG_HOME"/xmobar/xmobarrc
rofi_theme="$XDG_CONFIG_HOME"/rofi/theme.rasi
wezterm_cfg="$XDG_CONFIG_HOME"/wezterm/wezterm.lua

# FHD settings
declare -A FHD_cfg
FHD_cfg[screen_conf]=benq_XL2546X
FHD_cfg[dpi]=144
FHD_cfg[dmenu_fsize]=26
FHD_cfg[layout_hcenter]='0.8 1.0'
FHD_cfg[xmobar_dpi]=120
FHD_cfg[xmobar_pos]="28 0 140 0 0"
FHD_cfg[xmobar_font]="JetBrainsMono SZMDI 11"
FHD_cfg[rofi_yoffset]=200px
FHD_cfg[term_fontsize]=12.0
FHD_cfg[tray_h]=28
FHD_cfg[tray_w]=140
FHD_cfg[tray_p]=6

# QHD 1440p settings
declare -A QHD_cfg
QHD_cfg[screen_conf]=alienware_AW2725DF
QHD_cfg[dpi]=144
QHD_cfg[dmenu_fsize]=26
QHD_cfg[layout_hcenter]='0.8 1.0'
QHD_cfg[xmobar_dpi]=144
QHD_cfg[xmobar_pos]="30 0 200 0 0"
QHD_cfg[xmobar_font]="JetBrainsMono SZMDI 11"
QHD_cfg[rofi_yoffset]=400px
QHD_cfg[term_fontsize]=12.0
QHD_cfg[tray_h]=30
QHD_cfg[tray_w]=200
QHD_cfg[tray_p]=8

# 4K settings
declare -A UHD_cfg
UHD_cfg[screen_conf]=benq_RD280UA
UHD_cfg[dpi]=168
UHD_cfg[dmenu_fsize]=30
UHD_cfg[layout_hcenter]='0.8 0.9'
UHD_cfg[xmobar_dpi]=168
UHD_cfg[xmobar_pos]="40 0 240 0 0"
UHD_cfg[xmobar_font]="JetBrainsMono SZMDI 12"
UHD_cfg[rofi_yoffset]=1100px
UHD_cfg[term_fontsize]=14.0
UHD_cfg[tray_h]=40
UHD_cfg[tray_w]=240
UHD_cfg[tray_p]=10

log() {
  echo -e "$1"
}

setup() {
  local -n cfg=$1

  # switch xorg monitor conf
  cp $xconfdir/02-"${cfg[screen_conf]}".conf.bak $xconfdir/02-monitor_layout.conf
  log "  ${B_GRN}switched$RS to $B_YLW${cfg[screen_conf]}.conf$RS"

  # update xmonad config
  cp $xmonad_hs ${xmonad_hs}.bak
  sed -Ei "s|^dmenuFnSize =.+|dmenuFnSize = ${cfg[dmenu_fsize]}|" "$xmonad_hs"
  log "  ${B_GRN}updated$RS dmenu fontsize $B_YLW${cfg[dmenu_fsize]}$RS"
  sed -Ei "s|hCentered = bottomIfSingle.+|hCentered = bottomIfSingle ${cfg[layout_hcenter]}|" "$xmonad_hs"
  log "  ${B_GRN}updated$RS layout hCenter $B_YLW${cfg[layout_hcenter]}$RS"
  ## update systray
  sed -Ei "s|^trayW =.+|trayW = ${cfg[tray_w]}|" "$xmonad_hs"
  log "  ${B_GRN}updated$RS system-tray width $B_YLW${cfg[tray_w]}$RS"
  sed -Ei "s|^trayH =.+|trayH = ${cfg[tray_h]}|" "$xmonad_hs"
  log "  ${B_GRN}updated$RS system-tray height $B_YLW${cfg[tray_h]}$RS"
  sed -Ei "s|^trayP =.+|trayP = ${cfg[tray_p]}|" "$xmonad_hs"
  log "  ${B_GRN}updated$RS system-tray padding $B_YLW${cfg[tray_p]}$RS"
  delta -s --paging=never ${xmonad_hs}.bak $xmonad_hs || true
  log

  # update Xresources DPI
  sed -Ei "s|^Xft.dpi.+|Xft.dpi: ${cfg[dpi]}|" "$HOME"/.Xresources
  log "  ${B_GRN}updated$RS .Xresources DPI $B_YLW${cfg[dpi]}$RS"

  # update xmobar config
  cp "$xmobar_cfg" "$xmobar_cfg".bak
  sed -Ei "s|font = \".+\"|font = \"${cfg[xmobar_font]}\"|" "$xmobar_cfg"
  log "  ${B_GRN}updated$RS xmobar font $B_YLW${cfg[xmobar_font]}$RS"
  sed -Ei "s|dpi = .+|dpi = ${cfg[xmobar_dpi]}|" "$xmobar_cfg"
  log "  ${B_GRN}updated$RS xmobar DPI $B_YLW${cfg[xmobar_dpi]}$RS"
  sed -Ei "s|position = BottomHM.+|position = BottomHM ${cfg[xmobar_pos]}|" "$xmobar_cfg"
  log "  ${B_GRN}updated$RS xmobar position $B_YLW${cfg[xmobar_pos]}$RS"
  delta -s --paging=never "$xmobar_cfg".bak "$xmobar_cfg" || true
  log

  # update rofi config
  sed -Ei "s|y-offset: .+;|y-offset: ${cfg[rofi_yoffset]};|" "$rofi_theme"
  log "  ${B_GRN}updated$RS rofi yoffset $B_YLW${cfg[rofi_yoffset]}$RS"

  # update wezterm config
  cp "$wezterm_cfg" "$wezterm_cfg".bak
  sed -Ei "s|^c.dpi = .+|c.dpi = ${cfg[dpi]}|" "$wezterm_cfg"
  log "  ${B_GRN}updated$RS wezterm DPI $B_YLW${cfg[dpi]}$RS"
  sed -Ei "s|^c.font_size = .+|c.font_size = ${cfg[term_fontsize]}|" "$wezterm_cfg"
  log "  ${B_GRN}updated$RS wezterm font size $B_YLW${cfg[term_fontsize]}$RS"
  delta -s --paging=never "$wezterm_cfg".bak "$wezterm_cfg" || true
  log
}

res=$(printf '4K\nQHD\nFHD' | fzf --no-info --header="setup")
if [ -z "$res" ]; then
  exit 0
fi

case $res in
FHD)
  setup 'FHD_cfg'
  ;;
QHD)
  setup 'QHD_cfg'
  ;;
4K)
  setup 'UHD_cfg'
  ;;
esac

log "$B_GRN✓$RS ${BI_MGT}$res$RS"
log
