#!/bin/bash
# pierreD

# switch between custom and DoT DNS

set -e

resolv_cfg_dir="/etc/systemd/resolved.conf.d"

choice=$(printf 'tailnet\ncf DoT\ndeip DoT' | fzf --no-info --header="select DNS")
if [ -z "$choice" ]; then
  exit 0
fi

case $choice in
"tailnet")
  sudo cp $resolv_cfg_dir/tailnet.conf.bak $resolv_cfg_dir/dns.conf
  ;;
cf*)
  sudo cp $resolv_cfg_dir/cf_dot.conf.bak $resolv_cfg_dir/dns.conf
  ;;
deip*)
  sudo cp $resolv_cfg_dir/deip_dot.conf.bak $resolv_cfg_dir/dns.conf
  ;;
esac

sudo systemctl restart systemd-resolved.service
resolvectl status --no-pager
