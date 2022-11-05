#!/bin/bash

# find and kill process

set -e

red="\e[38;5;1m"
bold="\e[1m"
reset="\e[0m"

if ! fzf --version &> /dev/null; then
  >&2 printf "%bThis script needs %bfzf%b%b to work.%b\n" "$red" "$bold" "$reset" "$red" "$reset"
  exit 1
fi

pids=$(ps -eo user=user,pid=pid,ppid=ppid,%cpu=cpu,%mem=mem,args=cmd \
| fzf -m --header-lines=1 --preview 'echo {}' --preview-window=up:4:sharp:wrap \
| awk '{print $2}')

if [ -z "$pids" ]; then
  exit 0
fi

signal=$(printf 'SIGTERM\nSIGKILL\nSIGINT\nSIGABRT' | fzf --header="Signal")

for pid in ${pids}; do
  if ! kill -s "$signal" "$pid" &> /dev/null; then
    sudo kill -s "$signal" "$pid"
  fi
done
