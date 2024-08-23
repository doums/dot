#!/bin/bash

# Tailscale up/down wrapper

choices="up (exit linode)\nup\nstatus"
header="tailscale DOWN"
if tailscale status &>/dev/null; then
  choices="down\nstatus"
  header="tailscale UP"
fi

choice=$(echo -e "$choices" | fzf --header="$header")
# if no choice, show status
if [ -z "$choice" ]; then
  tailscale status
  exit 0
fi

case $choice in
"up (exit linode)")
  sudo tailscale up --exit-node=linode --exit-node-allow-lan-access=true
  tailscale status
  ;;
"up")
  sudo tailscale up --exit-node="" --exit-node-allow-lan-access=false
  tailscale status
  ;;
"down")
  sudo tailscale down
  tailscale status
  ;;
"status") tailscale status ;;
esac
