#!/bin/bash

# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at https://mozilla.org/MPL/2.0/.

pids=$(ps -o pid= -C picom)
for pid in "${pids[@]}"; do
  if [ -n "$pid" ]; then
    kill "$pid"
  fi
done

until picom --config /home/pierre/.config/picom.conf -b; do
  echo "picom restart fails"
done
echo "picom restart done"
