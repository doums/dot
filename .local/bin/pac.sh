#!/bin/bash

remove ()
{
  choice=$(pacman -Qq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder)
  sudo pacman -Rsn "$choice"
}

choice=$(printf "query\nforeign\nsync\nexplicitly\nremove\nupdate" | fzf --no-info)
case "$choice" in
  "query")
    pacman -Qq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder
  ;;
  "foreign")
    pacman -Qmq | fzf --preview 'pacman -Qil {}' --preview-window=right:70%:noborder
  ;;
  "sync")
    pacman -Ssq | fzf --preview 'pacman -Si {}'\
      --preview-window=right:70%:noborder\
      --bind "change:reload(pacman -Ssq)"
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
esac
