#!/bin/bash

# from port(s) find and kill process

set -e

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! fzf --version &> /dev/null; then
  >&2 printf "%bThis script needs %bfzf%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
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
