#!/usr/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

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

if [ ! "$1" ]; then
  git log --oneline --decorate=short | fzf \
    --preview="git show --color=always --abbrev-commit --pretty=medium --date=format:%c {1}" \
    --preview-window=right:70%:noborder \
    --header="git log"
else
  git log --oneline --parents --decorate=short --diff-filter=a -- "$1" | fzf \
    --with-nth=3.. \
    --preview="git diff -p --color=always --stat {2} {1} -- $1" \
    --preview-window=right:70%:noborder \
    --header="git log $1"
fi

