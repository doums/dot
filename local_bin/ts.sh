#!/bin/bash

# Tailscale up/down wrapper

choices="UP (exit linode)\nUP\nstatus"
header="tailscale DOWN"
if tailscale status &>/dev/null; then
  choices="exit node UP\nexit node DOWN\naccept dns ON\naccept dns OFF\nallow lan access ON\nallow lan access OFF\nDOWN\nstatus"
  header="tailscale UP"
fi

status() {
  tailscale status
  ip=$(curl -LfsS -4 icanhazip.com)
  echo -e "\npublic IP: $ip"
}

choice=$(echo -e "$choices" | fzf --header="$header")
# if no choice, show status
if [ -z "$choice" ]; then
  status
  exit 0
fi

case $choice in
"UP (exit linode)")
  sudo tailscale up --exit-node=linode --accept-dns --exit-node-allow-lan-access=true
  status
  ;;
"UP")
  sudo tailscale up --exit-node="" --accept-dns --exit-node-allow-lan-access=false
  status
  ;;
"exit node UP")
  sudo tailscale set --exit-node linode
  status
  ;;
"exit node DOWN")
  sudo tailscale set --exit-node ""
  status
  ;;
"accept dns ON")
  sudo tailscale set --accept-dns
  status
  ;;
"accept dns OFF")
  sudo tailscale set --accept-dns=false
  status
  ;;
"allow lan access ON")
  sudo tailscale set --exit-node-allow-lan-access
  status
  ;;
"allow lan access OFF")
  sudo tailscale set --exit-node-allow-lan-access=false
  status
  ;;
"DOWN")
  sudo tailscale down
  tailscale status
  ;;
"status") status ;;
esac
