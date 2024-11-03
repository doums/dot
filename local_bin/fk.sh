#!/bin/bash
# pierreD

# find and kill process(es) quickly

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
