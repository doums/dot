#!/bin/bash
# pierreD

# searching by port listened find and kill process(es) quickly

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

processes=$(ss -pnOatu \
| fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:4:sharp:wrap \
| awk '{print $7}')

if [ -z "$processes" ]; then
  exit 0
fi

signal=$(printf 'SIGTERM\nSIGKILL\nSIGINT\nSIGABRT' | fzf --header="Signal")

for process in ${processes}; do
  match=$(grep -o "pid=[[:digit:]]\+" <<< "${process}")
  pid="${match##pid=}"
  if [ "$pid" ]; then
    if ! kill -s "$signal" "$pid" &> /dev/null; then
      sudo kill -s "$signal" "$pid"
    fi
  fi
done
