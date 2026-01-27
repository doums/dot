#!/bin/bash

# restart stalonetray

set -e

log() {
  echo "[$0] $1"
}

pids=$(ps -o pid= -C stalonetray || true)
if [ -n "$pids" ]; then
  log 'killing stalonetray'
  killall -q stalonetray
else
  log '⚠ stalonetray is not running'
fi

if ! (stalonetray -c /home/pierre/.config/stalonetray/config &) then
  log '✗ stalonetray failed to start'
  exit 1
fi
log "✓ restarted stalonetray"
