--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

-- Base
import XMonad
import System.Exit
import qualified XMonad.StackSet as W

-- Data
import Data.Monoid
import qualified Data.Map as M

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll (killAll)

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition

-- Utils
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce

-- XF86 keys
import Graphics.X11.ExtraTypes.XF86

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal = "alacritty"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth = 4

-- Amount of space around windows
--
mySpacing :: Int
mySpacing = 7

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1", "2", "3", "a", "z", "e", "4", "r", "5"]
myWorkspaceKeys = [ xK_ampersand
                  , xK_eacute
                  , xK_quotedbl
                  , xK_a
                  , xK_z
                  , xK_e
                  , xK_apostrophe
                  , xK_r
                  , xK_parenleft ]

-- colors
--
grey = "#404040"
darkGrey = "#262626"
nearBlack = "#0d0d0d"
stone = "#8c8c8c"

-- dmenu args
--
dmenuFn = "'JetBrains Mono:pixelsize=22:antialias=true'"
dDarkGrey = "\\#262626"
dNearBlack = "\\#0d0d0d"
dStone = "\\#8c8c8c"

dmenuArgs :: String
dmenuArgs = " -fn " ++ dmenuFn ++ " -nb " ++ dNearBlack ++ " -nf " ++ dStone ++ " -sb " ++ dDarkGrey ++ " -sf " ++ dStone

-- toggle xmobar command
--
toggleXmobar :: String
toggleXmobar = "dbus-send --session --dest=org.Xmobar.Control --type=method_call '/org/Xmobar/Control' org.Xmobar.Control.SendSignal 'string:Toggle 0'"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm,                 xK_t),                     spawn $ XMonad.terminal conf)
    , ((modm,                 xK_exclam),                spawn "rofi -show drun")
    , ((modm,                 xK_w),                     spawn "rofi -show window")
    , ((modm .|. shiftMask,   xK_l),                     spawn "lock.sh")
    , ((modm,                 xK_q),                     spawn ("session.sh" ++ dmenuArgs))
    , ((modm .|. controlMask, xK_q),                     spawn "xmonad --recompile; xmonad --restart")
    , ((modm,                 xK_d),                     spawn ("set_dp.sh" ++ dmenuArgs))
    , ((0,                    xF86XK_MonBrightnessUp),   spawn "pral.sh light_up")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "pral.sh light_down")
    , ((0,                    xF86XK_AudioRaiseVolume),  spawn "pral.sh sink_up")
    , ((0,                    xF86XK_AudioLowerVolume),  spawn "pral.sh sink_down")
    , ((0,                    xF86XK_AudioMute),         spawn "pral.sh sink_mute")
    , ((0,                    xF86XK_AudioMicMute),      spawn "pral.sh source_mute")
    , ((modm,                 xK_asterisk),              spawn "pkill -USR1 redshift")
    , ((0,                    xK_Print),                 spawn "screenshot.sh")
    , ((modm,                 xK_c),                     spawn "clipshot.sh")
    , ((modm,                 xK_x),                     kill)
    , ((modm .|. shiftMask,   xK_x),                     killAll)
    , ((modm,                 xK_space),                 sendMessage NextLayout)
    , ((modm .|. controlMask, xK_r),                     refresh)
    , ((modm,                 xK_j),                     windows W.focusDown)
    , ((modm,                 xK_k),                     windows W.focusUp)
    , ((modm,                 xK_m),                     windows W.focusMaster)
    , ((modm,                 xK_Return),                windows W.swapMaster)
    , ((modm .|. shiftMask,   xK_j),                     windows W.swapDown)
    , ((modm .|. shiftMask,   xK_k),                     windows W.swapUp)
    , ((modm,                 xK_f),                     withFocused $ windows . W.sink)
    , ((modm,                 xK_Left),                  prevWS)
    , ((modm,                 xK_Right),                 nextWS)
    , ((modm .|. shiftMask,   xK_Left),                  shiftToPrev)
    , ((modm .|. shiftMask,   xK_Right),                 shiftToNext)
    , ((modm,                 xK_h),                     sendMessage Shrink)
    , ((modm,                 xK_l),                     sendMessage Expand)
    , ((modm,                 xK_semicolon),             sendMessage (IncMasterN 1))
    , ((modm,                 xK_comma),                 sendMessage (IncMasterN (-1)))
    , ((modm,                 xK_b),                     spawn toggleXmobar)
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        -- | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = renamed [CutWordsLeft 1]
           $ smartBorders
           $ avoidStruts
           $ spacingWithEdge mySpacing
           $ tiled ||| mirror ||| full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = renamed [Replace "→"] (Tall nmaster delta ratio)
     mirror  = renamed [Replace "↓"] (Mirror tiled)
     full    = renamed [Replace "■"] Full

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = insertPosition End Newer <+> composeAll
    [ className =? "Gimp"              --> doFloat
    , className =? "jetbrains-toolbox" --> doIgnore
    , className =? "feh"               --> doFloat
    , className =? "Galculator"        --> doFloat
    , className =? "TeamSpeak 3"       --> doFloat
    , resource  =? "desktop_window"    --> doIgnore ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
    setDefaultCursor xC_left_ptr -- set default cursor
    spawnOnce "picom --config /home/pierre/.config/picom.conf -b"
    spawnOnce "gnome-keyring-daemon --start -d"
    spawnOnce "redshift -c /home/pierre/.config/redshift/redshift.conf"
    spawnOnce "dunst -c /home/pierre/.config/dunst/dunstrc"
    spawnOnce "udiskie"
    spawnOnce "bato"

------------------------------------------------------------------------
-- xmobarPP
--
myXmobarPP :: PP
myXmobarPP = def
    { ppSep              = "  "
    , ppWsSep            = " "
    , ppCurrent          = stone . wrap (white "❱") ""
    , ppVisible          = stone . wrap "❯" ""
    , ppHidden           = stone . wrap (white "•") ""
    , ppHiddenNoWindows  = stone . wrap " " ""
    , ppVisibleNoWindows = Just $  stone . wrap "❯" ""
    , ppUrgent           = stone . wrap (red "⚡") ""
    , ppLayout           = white
    , ppOrder            = \[ws, l, w] -> [ws, l] -- \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras           = [] -- [logTitles formatFocused formatUnfocused]
    }
  where
    -- formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    -- formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    -- ppWindow :: String -> String
    -- ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    stone, red, white :: String -> String
    white    = xmobarColor "#ffffff" ""
    red      = xmobarColor "#bf616a" ""
    stone    = xmobarColor "#8c8c8c" ""

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     . withSB (statusBarProp "xmobar" (pure myXmobarPP))
     $ myConfig

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
myConfig = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = darkGrey,
        focusedBorderColor = grey,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

