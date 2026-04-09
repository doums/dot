## cpumode

A script to set Intel cpu perf mode for power saving and improved
battery life

Provides:

- a script to set scaling governor, EPP and EPB
- a systemd service to set the mode at boot
- a sleep hook to set the mode on resume after sleep
- an udev rule to set the mode on AC/battery switch

Install with `./install.sh`

To check current mode run

```
cpumode.sh
```

ref https://wiki.archlinux.org/title/CPU_frequency_scaling
