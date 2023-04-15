#!/bin/bash
# pierreD

# enhanced file finder with live preview

set -e

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

if ! fzf --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD fzf$RS to work"
  exit 1
fi

if ! fd --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD fd$RS to work"
  exit 1
fi

fd -H -t f -t l "$@" | fzf --preview 'echo {}' --preview-window=down:4:sharp:wrap
