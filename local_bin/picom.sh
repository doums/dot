#!/bin/bash

pids=$(ps -o pid= -C picom)
if [ -n "$pids" ]; then
  echo "⚠ picom is running, killing it"
  killall -q picom
  # sleep because picom takes a while to shutdown
  sleep 1
  echo "✓ picom killed"
fi

if [ -c /dev/nvidia0 ]; then
  echo "⚠ picom: NVIDIA GPU detected, running with force_vblank_sched=present=1"
  export PICOM_DEBUG=force_vblank_sched=present
fi

if ! picom --config /home/pierre/.config/picom.conf -b; then
  echo "✗ picom failed to start"
  exit 1
fi
echo "✓ picom started"
