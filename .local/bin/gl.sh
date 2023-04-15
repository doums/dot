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

if [ ! "$1" ]; then
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
else
  preview="git show --color --abbrev-commit -s --pretty=medium --date=format:%c {1} \
  && echo -e \n \
  && git diff --color {2} {1} -- $1"
  if [ "$use_delta" ]; then
    preview="$preview | $delta_command"
  fi
  git log --oneline --parents --decorate=short -- "$1" | fzf \
  --with-nth=3.. \
  --preview="$preview" \
  --preview-window=up,80%,noborder,border-bottom \
  --header="git log $1"
fi
