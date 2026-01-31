#!/bin/bash
# pierreD

# ANSI style codes
RED="\e[38;5;1m" # red
YLW="\e[38;5;3m" # yellow
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"
B_YLW="$BLD$YLW"

if ! fzf --version &>/dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD fzf$RS to work"
  exit 1
fi

remove() {
  package=$(pacman -Qq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder)
  if [ -n "$package" ]; then
    echo -e "Remove $B_RED$package$RS"
    sudo pacman -Rsn "$package"
  fi
}

install() {
  package=$(pacman -Ssq | fzf --preview 'pacman -Si {}' \
    --preview-window=right:70%:noborder --bind "change:reload(pacman -Ssq)")
  if [ -n "$package" ]; then
    echo -e "Install $B_YLW$package$RS"
    sudo pacman -S "$package"
  fi
}

orphans() {
  package=$(pacman -Qtd | fzf | awk '{print $1}')
  if [ -n "$package" ]; then
    echo -e "Remove $B_RED$package$RS"
    sudo pacman -Rsn "$package"
  fi
}

choices=(
  "query -Q"
  "search -S"
  "aur"
  "explicitly"
  "remove"
  "orphans"
)
choice=$(printf "%s\n" "${choices[@]}" | fzf --no-info)
case "$choice" in
"query -Q")
  pacman -Qq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder
  ;;
"search -S")
  install
  ;;
"aur")
  pacman -Qmq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder
  ;;
"explicitly")
  pacman -Qeq | fzf --preview 'pacman -Qil {}' \
    --preview-window=right:70%:noborder
  ;;
"remove")
  remove
  ;;
"orphans")
  orphans
  ;;
esac
