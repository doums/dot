## Pacman

### config

Edit `/etc/pacman.conf`, in `[options]` section

- uncomment `Color`
- set `ParallelDownloads = 5`
- add `ILoveCandy`

### custom hooks

First install `pacman-contrib` (provides `paccache`).

Move `hooks/*.hook` (from repo) into `/etc/pacman.d/hooks/`

`clean_cache.hook` use `paccache`.
It runs after every system update, package installation and
package removal. It will only keep one older version of the
installed packages, preventing the cache from growing
indefinitely.

→ https://wiki.archlinux.org/index.php/Pacman#Cleaning_the_package_cache

### improving compile time

in `/etc/makepkg.conf` uncomment the line

```
MAKEFLAGS="-j8"
```

replace 8 with the number of CPU cores

```
nproc
```

→ https://wiki.archlinux.org/index.php/Makepkg#Improving_compile_times
