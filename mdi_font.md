## nerd patched font

font patcher: https://github.com/ryanoasis/nerd-fonts#font-patcher

1. install `fontforge`
2. clone the repo

```shell
git clone --depth 1 https://github.com/ryanoasis/nerd-fonts.git
cd nerd-fonts
```

3. patch

```shell
./font-patcher --progressbars --careful --makegroups 4 --mdi path/to/fontToPatch.ttf
```

4. rename font using FontForge
