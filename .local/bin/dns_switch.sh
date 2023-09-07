#!/bin/bash
# pierreD

# switch between custom and DoT DNS

set -e

resolv_cfg_dir="/etc/systemd/resolved.conf.d"

choice=$(printf 'custom\nDoT' | fzf --no-info --header="select DNS")
if [ -z "$choice" ]; then
  exit 0
fi

case $choice in
  "custom")
    sudo cp $resolv_cfg_dir/dns_tailnet.conf.bak $resolv_cfg_dir/dns.conf
  ;;
  "DoT")
    sudo cp $resolv_cfg_dir/dot.conf.bak $resolv_cfg_dir/dns.conf
  ;;
esac

sudo systemctl restart systemd-resolved.service
resolvectl status --no-pager

