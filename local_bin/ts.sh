#!/bin/bash

# Tailscale up/down wrapper
# required the current user to be set as operator:
# sudo tailscale set --operator=pierre

choices=('UP (exit linode)' 'UP' 'status')
header="tailscale DOWN"
if tailscale status &>/dev/null; then
  choices=(
    'exit node UP'
    'exit node DOWN'
    'accept dns ON'
    'accept dns OFF'
    'allow lan access ON'
    'allow lan access OFF'
    'DOWN'
    'status'
  )
  header="tailscale UP"
fi

status() {
  tailscale status
  ip=$(curl -LfsS -4 icanhazip.com)
  echo -e "\npublic IP: $ip"
}

choice=$(printf "%s\n" "${choices[@]}" | fzf --header="$header")
# if no choice, show status
if [ -z "$choice" ]; then
  status
  exit 0
fi

case $choice in
"UP (exit linode)")
  tailscale up --exit-node=linode --accept-dns --exit-node-allow-lan-access=true
  status
  ;;
"UP")
  tailscale up --exit-node="" --accept-dns --exit-node-allow-lan-access=false
  status
  ;;
"exit node UP")
  tailscale set --exit-node linode
  status
  ;;
"exit node DOWN")
  tailscale set --exit-node ""
  status
  ;;
"accept dns ON")
  tailscale set --accept-dns
  status
  ;;
"accept dns OFF")
  tailscale set --accept-dns=false
  status
  ;;
"allow lan access ON")
  tailscale set --exit-node-allow-lan-access
  status
  ;;
"allow lan access OFF")
  tailscale set --exit-node-allow-lan-access=false
  status
  ;;
"DOWN")
  tailscale down
  tailscale status
  ;;
"status") status ;;
esac
