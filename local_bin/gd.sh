#!/bin/bash
# pierreD

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# gd, enhanced git diff

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

if [ ! "$1" ]; then
  preview="git diff --color -- {}"
  if [ "$use_delta" ]; then
    preview="$preview | $delta_command"
  fi
  output=$(git diff --name-only | fzf \
    --preview="$preview" \
    --preview-window=up,80%,noborder,border-bottom \
    --header="git diff")
  if [ -n "$output" ]; then
    git diff --stat -- "$output"
  fi
fi
