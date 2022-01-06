#!/usr/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

# gd, enhanced git diff

if ! fzf --version &> /dev/null; then
  printf "%s\n" "fzf not found"
  exit 1
fi

if ! bat --version &> /dev/null; then
  printf "%s\n" "bat not found"
  exit 1
fi

if ! git status > /dev/null; then
  exit 1
fi

if delta --version &> /dev/null; then
  use_delta=true
  delta_command="delta -s -w ${FZF_PREVIEW_COLUMNS:-$COLUMNS}"
fi

if [ ! "$1" ]; then
  preview="git diff --color -- {}"
  if [ $use_delta ]; then
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
