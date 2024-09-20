## nerd patched font

font patcher: https://github.com/ryanoasis/nerd-fonts#font-patcher

1. install `fontforge` (arch package)
2. download font-patcher from latest release
3. patch

```shell
./font-patcher --progressbars --careful --makegroups 4 --mdi path/to/fontToPatch.ttf
```

4. rename font using FontForge

Element → Font Info → PS Names, TTF Names\
File → Generate Fonts

