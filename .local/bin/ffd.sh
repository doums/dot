#!/bin/bash

# find a file

set -e

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! fzf --version &> /dev/null; then
  >&2 printf "%bThis script needs %bfzf%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

if ! fd --version &> /dev/null; then
  >&2 printf "%bThis script needs %bfd%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

fd -H -t f -t l "$@" | fzf --preview 'echo {}' --preview-window=down:4:sharp:wrap
