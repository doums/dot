#!/bin/bash

red="\e[38;5;1m"
yellow="\e[38;5;3m"
bold="\e[1m"
reset="\e[0m"

remove ()
{
  package=$(pacman -Qq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder)
  if [ -n "$package" ]; then
    printf "Remove %b%b%s%b\n" "$bold" "$red" "$package" "$reset"
    sudo pacman -Rsn "$package"
  fi
}

install ()
{
  package=$(pacman -Ssq | fzf --preview 'pacman -Si {}'\
    --preview-window=right:70%:noborder\
    --bind "change:reload(pacman -Ssq)")
  if [ -n "$package" ]; then
    printf "Install %b%b%s%b\n" "$bold" "$yellow" "$package" "$reset"
    sudo pacman -S "$package"
  fi
}

orphans ()
{
  package=$(pacman -Qtd | fzf | awk '{print $1}')
  if [ -n "$package" ]; then
    printf "Remove %b%b%s%b\n" "$bold" "$red" "$package" "$reset"
    sudo pacman -Rsn "$package"
  fi
}

choice=$(printf "query\nforeign\nsync db\nexplicitly\nremove\nupdate\norphans" | fzf --no-info)
case "$choice" in
  "query")
    pacman -Qq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder
  ;;
  "foreign")
    pacman -Qmq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder
  ;;
  "sync db")
    install
  ;;
  "explicitly")
    pacman -Qeq | fzf --preview 'pacman -Qil {}'\
      --preview-window=right:70%:noborder
  ;;
  "remove")
    remove
  ;;
  "update")
    sudo pacman -Syu
  ;;
  "orphans")
    orphans
  ;;
esac
