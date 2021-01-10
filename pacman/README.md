## pacman stuff

Includes the main pacman configuration file and a post-transaction hook that runs after every system update, package installation and package removal. It will only keep one older version of the installed packages, preventing the cache from growing indefinitely.

- move `pacman.conf` in `/etc`
- move `clean_cache.hook` in `/etc/pacman.d/hooks`

The hook use `paccache` which is an util from the `pacman-contrib` package:
```
sudo pacman -S pacman-contrib
```

source: https://wiki.archlinux.org/index.php/Pacman#Cleaning_the_package_cache

### improving compile time
in `/etc/makepkg.conf` uncomment the line
```
MAKEFLAGS="-j8"
```
replace 8 with the number of CPU cores
```
nproc
```

source: https://wiki.archlinux.org/index.php/Makepkg#Improving_compile_times
