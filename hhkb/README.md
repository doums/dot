## HHKB XKB layout

A custom FR layout mixing Azerty and Qwerty, improved for coding

```
  ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
  │Esc│ & │ é │ " │ ' │ % │ - │ è │ _ │ ( │ ) │ * │ = │ \ │ ~ │
  ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤
  │Tab  │ a │ z │ e │ r │ t │ y │ u │ i │ o │ p │ [ │ ] │ BS  │
  ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴─────┤
  │Ctrl  │ q │ s │ d │ f │ g │ h │ j │ k │ l │ m │ $ │ Enter  │
  ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────┬───┤
  │Shift   │ w │ x │ c │ v │ b │ n │ , │ ; │ : │ ! │Shift │ ≡ │
  └─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴──┬┴───┴┬──┴┬─────┴───┘
        │Gui│ Alt │         Space         │AltGr│Gui│
        └───┴─────┴───────────────────────┴─────┴───┘
```

### Shifted

```
  ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
  │Esc│ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9 │ 0 │ ° │ + │ | │ ^ │
  ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤
  │Tab  │ A │ Z │ E │ R │ T │ Y │ U │ I │ O │ P │ { │ } │ BS  │
  ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴─────┤
  │Ctrl  │ Q │ S │ D │ F │ G │ H │ J │ K │ L │ M │ ` │ Enter  │
  ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────┬───┤
  │Shift   │ W │ X │ C │ V │ B │ N │ ? │ . │ / │ / │Shift │ ≡ │
  └─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴──┬┴───┴┬──┴┬─────┴───┘
        │Gui│ Alt │         Space         │AltGr│Gui│
        └───┴─────┴───────────────────────┴─────┴───┘
```

### AltGr'ed

```
  ┌───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───┐
  │Esc│ ¹ │ ² │ # │   │   │ ⌃ │ ` │ § │   │ @ │   │   │   │   │
  ├───┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴───┤
  │Tab  │ à │   │ ê │   │   │   │ ù │ î │ ô │   │   │   │ BS  │
  ├─────┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴┬──┴─────┤
  │Ctrl  │   │   │   │   │   │   │   │   │   │ µ │   │ Enter  │
  ├──────┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴─┬─┴────┬───┤
  │Shift   │   │   │ ç │   │   │   │ < │ > │   │   │Shift │ ≡ │
  └─────┬──┴┬──┴──┬┴───┴───┴───┴───┴───┴──┬┴───┴┬──┴┬─────┴───┘
        │Gui│ Alt │         Space         │AltGr│Gui│
        └───┴─────┴───────────────────────┴─────┴───┘
```

This is a personal X keyboard extension (XKB) layout. It is a
custom layout purely dedicated to my Happy Hacking Keyboard
Pro 2 model (by PFU).

All unnecessary stuff has been cleaned (thanks to my reverse
engineering ego-trip in the XKB rabbit hole).

#### Features:

- AZERTY for alpha keys
- specific French letters and accents
- using good QWERTY keys
- improved for code

### Install

```
sudo ./install.sh
```

**NOTE** The installer adds an udev rule that set the layout
specifically for the HHKB when it is plugged in via USB cable

#### Change the default layout

To change the default layout copy `conf/00-keyboard.conf` into
`/etc/X11/xorg.conf.d/`

**NOTE** Need to restart X to apply the change

