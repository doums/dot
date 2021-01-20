## Suspend service

In order to automatically lock the screen when suspending to RAM put this unit file in `/etc/systemd/system/` and enable `suspend@pierre` service
```
sudo systemctl enable suspend@pierre
```

source: https://wiki.archlinux.org/index.php/Power_management#Sleep_hooks
