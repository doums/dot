#!/bin/bash

set -e

echo "installing cpumode…"
sudo install -Dm755 cpumode.sh /usr/local/bin/cpumode.sh
sudo install -Dm644 cpumode.service /etc/systemd/system/cpumode.service
sudo install -Dm755 99-cpumode /usr/lib/systemd/system-sleep/99-cpumode
sudo install -Dm644 99-cpumode.rules /etc/udev/rules.d/99-cpumode.rules

echo "enabling cpumode service…"
sudo systemctl enable --now cpumode.service

echo "DONE"
