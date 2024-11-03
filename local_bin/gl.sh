#!/bin/bash
# pierreD

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# gl, enhanced git log

# ANSI style codes
RED="\e[38;5;1m" # red
BLD="\e[1m"      # bold
RS="\e[0m"       # style reset
B_RED="$BLD$RED"

if ! fzf --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD fzf$RS to work"
  exit 1
fi

if ! bat --version &> /dev/null; then
  >&2 echo -e " $B_RED⚠$RS This script needs$BLD bat$RS to work"
  exit 1
fi

if delta --version &> /dev/null; then
  use_delta=true
  delta_command="delta -s -w ${FZF_PREVIEW_COLUMNS:-$COLUMNS}"
fi

log() {
  preview="git show --color --abbrev-commit --pretty=medium --date=format:%c {1}"
  if [ "$use_delta" ]; then
    preview="$preview | $delta_command"
  fi
  output=$(git log --oneline --decorate=short | fzf \
    --preview="$preview" \
    --preview-window=up,80%,noborder,border-bottom \
    --header="git log" | awk '{print $1}')
  if [ -n "$output" ]; then
    git show "$output"
  fi
}

log_args=(--oneline --parents --decorate=short)
log_path() {
  preview="git show --color --abbrev-commit -s --pretty=medium --date=format:%c {1} \
  && echo -e \n \
  && git diff --color {2} {1} -- $1"
  if [ "$use_delta" ]; then
    preview="$preview | $delta_command"
  fi
  # if a revision-range is provided, add it to git log args
  if [ -n "$2" ]; then
    log_args+=("$2")
  fi
  git log "${log_args[@]}" -- "$1" | fzf \
  --with-nth=3.. \
  --preview="$preview" \
  --preview-window=up,80%,noborder,border-bottom \
  --header="git log $1 $2"
}

# $1 path (optional)
# $2 revision-range (optional)
if [ "$#" -eq 0 ]; then
  log
elif [ "$#" -eq 1 ]; then
  log_path "$1"
else
  log_path "$1" "$2"
fi
