import XMonad
import XMonad.Util.EZConfig
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Monoid
import Data.Char
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Navigation2D
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.WorkspaceHistory (workspaceHistoryHook)
import XMonad.Actions.CycleWorkspaceByScreen
import XMonad.Actions.PerWorkspaceKeys
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.Hacks as Hacks
import Graphics.X11.ExtraTypes.XF86

main :: IO ()
main = xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     . withSB (statusBarProp "xmobar" (pure bar))
     . withNavigation2DConfig nav2DConfig
     . nav2D
     $ cfg

cfg = def {
  terminal           = myTerminal,
  focusFollowsMouse  = True,
  clickJustFocuses   = False,
  borderWidth        = 6,
  modMask            = modm,
  workspaces         = wspaces,
  normalBorderColor  = darkGrey,
  focusedBorderColor = grey,
  layoutHook         = layouts,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  logHook            = myLogHook,
  startupHook        = myStartupHook
  }
  `additionalKeysP`         keybinds
  `removeKeysP`             kbRemoved
  `additionalMouseBindings` mousebinds

-- # Keymap
keybinds = ([
  -- ## Basics
  -- Recompile and restart XMonad
    ("M-C-q",       spawn "xmonad --recompile; xmonad --restart")
  -- Refresh XMonad
  , ("M-C-r",       refresh)
  -- Kill current window
  , ("M-x",         kill)

  -- ## Workspace navigation
  -- "M-<Workspace key>" Move to workspace x
  -- "M-S-<Workspace key>" Move current window to workspace x
  -- Switch to last workspace
  -- , ("M-<Tab>",     toggleRecentWS)
  , ("M-<Tab>",     cycleWorkspaceOnCurrentScreen [xK_Super_L] xK_h xK_l)
  -- Switch to next workspace
  , ("M-C-<Right>", nextWS)
  -- Switch to previous workspace
  , ("M-C-<Left>",  prevWS)
  -- Exec the action of the current workspace
  , ("M-<Return>",  chooseAction wsActions)

  -- ## Window navigation
  -- "M-↑→↓←" Navigate through windows
  -- "M-S-↑→↓←" Swap windows
  -- Focus next window up
  , ("M-k",         windows W.focusUp)
  -- Focus next window down
  , ("M-j",         windows W.focusDown)
  -- Focus master
  , ("M-m",         windows W.focusMaster)
  -- Swap to master
  , ("M-S-m",       windows W.swapMaster)
  -- Swap window up
  , ("M-S-k",       windows W.swapUp)
  -- Swap window down
  , ("M-S-j",       windows W.swapDown)
  -- Shift current window to next workspace
  , ("M-S-<Page_Up>",
                    shiftToNext)
  -- Shift current window to previous workspace
  , ("M-S-<Page_Down>",
                    shiftToPrev)

  -- ## Screen navigation
  -- "M-<[]>" Move to next/previous screen
  -- "M-S-<[]>" Move current window to next/previous screen

  -- ## Layout
  -- Next layout
  , ("M-<Space>",   sendMessage NextLayout)
  -- Unfloat current window
  , ("M-f",         withFocused $ windows . W.sink)
  -- Expand master area
  , ("M-l",         sendMessage Expand)
  -- Shrink master area
  , ("M-h",         sendMessage Shrink)
  -- Increment master slots
  , ("M-;",         sendMessage (IncMasterN 1))
  -- Decrement master slots
  , ("M-,",         sendMessage (IncMasterN (-1)))
  -- Fullscreen mode
  , ("M-S-f",       (toggleScreenSpacingEnabled
                      >> toggleWindowSpacingEnabled
                      >> sendMessage ToggleStruts))

  -- ## Launch apps
  -- Open a terminal
  , ("M-t",         spawn $ myTerminal)
  -- Open the application launcher
  , ("M-!",         spawn "rofi -show drun")
  -- Open the window switcher
  , ("M-w",         spawn "rofi -show window")
  -- Open session menu
  , ("M-q",         spawn ("session.sh" ++ dmenuArgs))
  -- Open glyph menu
  , ("M-g",         spawn ("glyph.sh" ++ dmenuArgs))
  -- Open ssh.sh
  , ("M-S-s",       spawn ("ssh.sh" ++ dmenuArgs))
  -- Open vm.sh
  , ("M-S-v",       spawn ("vm.sh" ++ dmenuArgs))
  -- Open clipboard manager
  , ("M-v",         spawn ("clipmenu -b -i -p '◧'" ++ dmenuArgs))
  -- Restart compositor
  , ("M-p",         spawn "picom.sh")
  -- Toggle Redshift
  , ("M-*",         spawn "pkill -USR1 redshift")
  -- Take a screenshot
  , ("<Print>",     spawn "screenshot.sh")
  -- Take a screenshot with square selection
  , ("M-c",         spawn "clipshot.sh")

  -- ## Scratchpads
  -- Keymap
  , ("M-S-,", namedScratchpadAction scratchpads "keymap")
  -- Take notes
  , ("M-:", namedScratchpadAction scratchpads "notes")
  -- Translate
  , ("M-n", namedScratchpadAction scratchpads "translate")
  -- Audio mixer
  , ("M-o", namedScratchpadAction scratchpads "pavucontrol")
  -- File manager
  , ("M-b", namedScratchpadAction scratchpads "file-manager")
  -- Calculator
  , ("M-=", namedScratchpadAction scratchpads "calc")
  -- IRC
  , ("M-i", namedScratchpadAction scratchpads "irc-chat")
  -- System monitor
  , ("M-s", namedScratchpadAction scratchpads "bottom")

  -- ## Multimedia
  -- Light up
  , ("<XF86MonBrightnessUp>",   spawn "pral.sh light_up")
  -- Light down
  , ("<XF86MonBrightnessDown>", spawn "pral.sh light_down")
  -- Volume up
  , ("<XF86AudioRaiseVolume>",  spawn "pral.sh sink_up")
  -- Volume down
  , ("<XF86AudioLowerVolume>",  spawn "pral.sh sink_down")
  -- Mute audio
  , ("<XF86AudioMute>",         spawn "pral.sh sink_mute")
  -- Mute mic
  , ("<XF86AudioMicMute>",      spawn "pral.sh source_mute") ]
  ++
  [ (m ++ "M-" ++ [k], windows $ f i)
    | (i, k) <- zip wspaces wsKeys
    , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]
  ])

-- ## Mouse bindings
-- "M-<left click>" Drag the window and make it floating
-- "M-<right click>" Resize the window (make it floating)
mousebinds = [
  ((modm, button1), \w -> focus w >> mouseMoveWindow w)
  , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w) ]

nav2DConfig = def {layoutNavigation = [("→", sideNavigation)
                                     , ("↓", sideNavigation)]}
nav2D = navigation2DP def ("<Up>", "<Left>", "<Down>", "<Right>")
                          [("M-", windowGo), ("M-S-", windowSwap)]
                          True
-- #

kbRemoved = ["M-S-c", "M-S-q"]

modm = mod4Mask
myTerminal = "wezterm"
-- workspaces
-- 󰞷 󰖟 󱃖 󰭹 z 󰅶 4 r 5
wspaces = ["\985015", "\984479", "\987350", "\985977", "z", "\983414", "4", "5", "r"]
wsKeys = "&é\"aze'%r"
screenKeys = "[]"

grey = "#404040"
darkGrey = "#262626"
stone = "#8c8c8c"

-- dmenu config
dmenuFnSize = 31
dmenuFn = "'JetBrainsMono:pixelsize=" ++ show dmenuFnSize ++ ":antialias=true'"
dDarkGrey = "\\#262626"
dNearBlack = "\\#0d0d0d"
dStone = "\\#8c8c8c"
dmenuArgs = " -fn " ++ dmenuFn ++ " -nb " ++ dNearBlack ++ " -nf " ++ dStone ++ " -sb " ++ dDarkGrey ++ " -sf " ++ dStone

layouts = lessBorders Screen
           $ spaced (tiled ||| mirror) ||| full
  where
    tiled    = renamed [Replace "→"] (Tall nmaster delta ratio)
    mirror   = renamed [Replace "↓"] (Mirror tiled)
    full     = renamed [Replace "■"] Full
    spaced l = renamed [CutWordsLeft 1] $ avoidStruts $ spacingWithEdge 9 $ l
    nmaster  = 1 -- default number of windows in the master pane
    ratio    = 1/2 -- default proportion of screen occupied by master pane
    delta    = 3/100 -- percent of screen to increment by when resizing panes

myManageHook = fmap not willFloat --> insertPosition Below Newer
               <+> namedScratchpadManageHook scratchpads
               <+> composeAll
    [ isDialog                          --> doFloat
    , className =? "stalonetray"        --> doIgnore
    , className =? "feh"                --> doFloat
    , className =? "jetbrains-toolbox"  --> doCenterFloat
    , className =? "Gpick"              --> doFloat
    , className =? "steam"              --> doFloat
    , title =? "splash"
        <&&> className ^? "jetbrains-"  --> doCenterFloat <> hasBorder False
    , title ^? "Emulator"               --> doFloat
    , (className =? "firefox" <&&> resource =? "Dialog")
                                        --> doFloat
    , title     =? "Volume Control"     --> doCenterFloat
    , resource  =? "desktop_window"     --> doIgnore
    , title =? "NymVPN"                 --> doFloat
    , appName =? "virt-manager"         --> doFloat
    , appName =? "virt-viewer"          --> doFloat
    , (role ^? "gimp-toolbox" <||> role =? "gimp-image-window")
                                        --> doFloat
    ]
    where role = stringProperty "WM_WINDOW_ROLE"

myEventHook = handleEventHook def
                -- <> Hacks.trayerAboveXmobarEventHook
                <> Hacks.windowedFullscreenFixEventHook

myLogHook = workspaceHistoryHook

myStartupHook = do
    setDefaultCursor xC_left_ptr -- set default cursor
    spawnOnce "xwallpaper --daemon --clear --zoom $BG_PRIMARY"
    spawnOnce "picom.sh"
    spawnOnce "dunst"
    spawnOnce "udiskie"
    spawnOnce "bato"
    spawnOnce "solaar -w hide"
    spawnOnce "clipmenud"
    spawnOnce "redshift -c /home/pierre/.config/redshift/redshift.conf"
    spawnOnce "stalonetray -c /home/pierre/.config/stalonetray/config"
    spawnOnce "x-on-resize --display ':0' --resize '/home/pierre/.local/bin/restart_stalonetray.sh'"

-- xmobar
bar = def
    { ppSep              = "  "
    , ppWsSep            = " "
    , ppCurrent          = stone . wrap (white "❱") ""
    , ppVisible          = stone . wrap "❯" ""
    , ppHidden           = stone . wrap (white "•") ""
    , ppHiddenNoWindows  = stone . wrap " " ""
    , ppVisibleNoWindows = Just $  stone . wrap "❯" ""
    , ppUrgent           = stone . wrap (red "⚡") ""
    , ppLayout           = white . wrap (stone "⌈") (stone "⌋")
    , ppOrder            = \[ws, l, w] -> [ws, l]
    , ppExtras           = []
    }
  where
    stone, red, white :: String -> String
    white    = xmobarColor "#ffffff" ""
    red      = xmobarColor "#bf616a" ""
    stone    = xmobarColor "#8c8c8c" ""

-- workspaces dedicated action
wsActions ws = case ws of
  -- 󰞷
  "\985015" -> spawn $ myTerminal
  -- 󰖟
  "\984479" -> spawn "firefox"
  -- 󱃖
  "\987350" -> spawn $ "jetbrains-toolbox"
  -- 󰭹
  "\985977" -> spawn "org.zulip.Zulip"
  _         -> undefined

-- helper to define terminal commands for scratchpads
spTerm className cmd = termCmd
                      ++ className
                      ++ " -- "
                      ++ cmd
  where termCmd = "wezterm --config enable_tab_bar=false start --class "

-- scratchpads
-- RationalRect `x y width height` - from top left corner, 0-1
-- 1 means full width/height of the screen
scratchpads =
  [ NS "translate" (spTerm "translate" "gtt")
      (appName =? "translate")
      (customFloating $ W.RationalRect 0.18 0.15 0.7 0.6)
  , NS "pavucontrol" "pavucontrol" (className =? "pavucontrol")
      (customFloating $ W.RationalRect 0.2 0.2 0.5 0.6)
  , NS "keymap" "apekey"
      (title =? "apekey")
      (customFloating $ W.RationalRect 0.30 0.08 0.38 0.8)
  , NS "file-manager" (spTerm "file-manager" "nnn")
      (appName =? "file-manager")
      (customFloating $ W.RationalRect (1/4) (1/6) (3/9) (4/6))
  , NS "irc-chat" (spTerm "irc-chat" "tiny")
      (appName =? "irc-chat")
      (doFloatAt 0.2 0.1)
  , NS "bottom" (spTerm "systemMonitor" "btm")
      (appName =? "systemMonitor")
      (customFloating $ W.RationalRect (0.08) (0.06) (0.85) (0.85))
  , NS "calc" (spTerm "calc" "kalker")
      (appName =? "calc")
      (customFloating $ W.RationalRect (1/4) 0.2 (1/4) (3/6))
  , NS "notes" (spTerm "notes" "nvim /home/pierre/.local/share/notes.md")
      (appName =? "notes")
      (doFloatAt 0.2 0.15)
  ]

