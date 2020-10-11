#!/usr/bin/bash

mapfile -t pids <<< "$(pgrep -x bato)"
if [ "${#pids[@]}" -gt 0 ]; then
  for pid in "${pids[@]}"; do
    if [ -n "$pid" ]; then
      kill "$pid"
    fi
  done
fi
bato &
