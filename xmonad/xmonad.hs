import XMonad
import XMonad.Util.EZConfig
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Monoid
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Navigation2D
import Data.Char
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
import XMonad.Hooks.WorkspaceHistory
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
  borderWidth        = 4,
  modMask            = modm,
  workspaces         = myWorkspaces,
  normalBorderColor  = darkGrey,
  focusedBorderColor = grey,
  layoutHook         = myLayout,
  manageHook         = myManageHook,
  handleEventHook    = myEventHook,
  logHook            = myLogHook,
  startupHook        = myStartupHook
  }
  `additionalKeysP`         keybinds
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
  , ("M-<Tab>",       toggleRecentWS)
  -- Switch to next workspace
  , ("M-<Page_Up>",   nextWS)
  -- Switch to previous workspace
  , ("M-<Page_Down>", prevWS)
  -- Exec the action of the current workspace
  , ("M-<Return>",    chooseAction wsActions)

  -- ## Window navigation
  -- "M-↑→↓←" Navigate through windows
  -- "M-S-↑→↓←" Swap windows
  -- Focus next window up
  , ("M-k",         windows W.focusUp)
  -- Focus next window down
  , ("M-j",         windows W.focusDown)
  -- Focus master
  , ("M-m",         windows W.focusMaster)
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

  -- ## Launch apps
  -- Open a terminal
  , ("M-t",         spawn $ myTerminal)
  -- Open the application launcher
  , ("M-!",         spawn "rofi -show drun")
  -- Open the window switcher
  , ("M-w",         spawn "rofi -show window")
  -- Screen lock
  , ("M-S-l",       spawn "lock.sh")
  -- Open session menu
  , ("M-q",         spawn ("session.sh" ++ dmenuArgs))
  -- Open glyph menu
  , ("M-g",         spawn ("glyph.sh" ++ dmenuArgs))
  -- Open clipboard manager
  , ("M-v",         spawn ("clipmenu -b -i -p '◧'" ++ dmenuArgs))
  -- Restart compositor
  , ("M-p",         spawn "restart_picom.sh")
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
  -- Network Manager
  , ("M-u", namedScratchpadAction scratchpads "network")
  -- Insomnia
  , ("M-S-i", namedScratchpadAction scratchpads "insomnia")

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
  [ (m ++ "M-" ++ [k], f i)
      | (i, k) <- zip myWorkspaces workspaceKeys
      , (f, m) <- [(windows . W.greedyView, ""), (windows . W.shift, "S-")] ]
  ++
  [ (m ++ "M-" ++ [k], screenWorkspace sc >>= flip whenJust f)
      | (k, sc) <- zip screenKeys [0..]
      , (f, m) <- [(windows . W.view, ""), (windows . W.shift, "S-")] ]
  )

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

modm = mod4Mask
myTerminal = "alacritty"
-- workspaces: 󰞷 󰖟 󱃖 󰭹 z 󰅶 4 r 5
myWorkspaces = ["\985015", "\984479", "\987350", "\985977", "z", "\983414", "4", "r", "5"]
workspaceKeys = "&é\"aze'r("
screenKeys = "[]"

grey = "#404040"
darkGrey = "#262626"
stone = "#8c8c8c"

-- dmenu config
dmenuFn = "'Roboto Condensed:pixelsize=22:antialias=true'"
dDarkGrey = "\\#262626"
dNearBlack = "\\#0d0d0d"
dStone = "\\#8c8c8c"
dmenuArgs = " -fn " ++ dmenuFn ++ " -nb " ++ dNearBlack ++ " -nf " ++ dStone ++ " -sb " ++ dDarkGrey ++ " -sf " ++ dStone

myLayout = renamed [CutWordsLeft 1]
           $ smartBorders
           $ avoidStruts
           $ spacingWithEdge 7
           $ tiled ||| mirror ||| full
  where
    tiled   = renamed [Replace "→"] (Tall nmaster delta ratio)
    mirror  = renamed [Replace "↓"] (Mirror tiled)
    full    = renamed [Replace "■"] Full
    nmaster = 1 -- The default number of windows in the master pane
    ratio   = 1/2 -- Default proportion of screen occupied by master pane
    delta   = 3/100 -- Percent of screen to increment by when resizing panes

myManageHook = fmap not willFloat --> insertPosition Below Newer
               <+> namedScratchpadManageHook scratchpads
               <+> composeAll
    [ className =? "Gimp"               --> doFloat
    , className =? "feh"                --> doFloat
    , className =? "jetbrains-toolbox"  --> doCenterFloat
    , title =? "splash"
        <&&> className ^? "jetbrains-"  --> doCenterFloat <> hasBorder False
    , title ^? "Emulator"               --> doFloat
    , (className =? "firefox" <&&> resource =? "Dialog")
                                        --> doFloat
    , title     =? "Volume Control"     --> doCenterFloat
    , resource  =? "desktop_window"     --> doIgnore ]

myEventHook = handleEventHook def
                <> Hacks.trayerAboveXmobarEventHook
                <> Hacks.windowedFullscreenFixEventHook

myLogHook = workspaceHistoryHook

myStartupHook = do
    setDefaultCursor xC_left_ptr -- set default cursor
    spawnOnce "picom --config /home/pierre/.config/picom.conf -b"
    spawnOnce "redshift -c /home/pierre/.config/redshift/redshift.conf"
    spawnOnce "dunst -c /home/pierre/.config/dunst/dunstrc"
    spawnOnce "udiskie"
    -- spawnOnce "bato"
    spawnOnce "solaar -w hide"
    spawnOnce "clipmenud"
    spawnOnce "trayer -l --align left --distancefrom left --distance 540 --monitor primary --widthtype request --height 28 --transparent true --alpha 0 --tint 0x262626 --expand true --iconspacing 4 --SetPartialStrut true --SetDockType true"

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
  "\985977" -> spawn "im.riot.Riot --profile nym"
  _         -> undefined

-- scratchpads
-- RationalRect `x y width height` - from top left corner, 0-1
-- 1 means full width/height of the screen
scratchpads =
  [ NS "translate" "alacritty --class translate -e gtt"
      (appName =? "translate")
      (customFloating $ W.RationalRect 0.25 0.25 0.6 0.4)
  , NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
      (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
  , NS "keymap" "apekey"
      (appName =? "apekey")
      (customFloating $ W.RationalRect 0.38 0.12 0.33 0.8)
  , NS "file-manager" "alacritty --class file-manager -e nnn"
      (appName =? "file-manager")
      (customFloating $ W.RationalRect (1/4) (1/6) (3/9) (4/6))
  , NS "irc-chat" "alacritty --class irc-chat -e tiny"
      (appName =? "irc-chat")
      (doFloatAt 0.2 0.3)
  , NS "bottom" "alacritty --class systemMonitor -e btm"
      (appName =? "systemMonitor")
      (customFloating $ W.RationalRect (1/6) (1/9) (4/6) (7/9))
  , NS "calc" "alacritty --class calc -e kalker"
      (appName =? "calc")
      (customFloating $ W.RationalRect (1/4) (2/6) (1/4) (3/6))
  , NS "notes" "alacritty -o window.dimensions.columns=78 --class notes -e nvim /home/pierre/.local/share/notes.md"
      (appName =? "notes")
      (doFloatAt 0.2 0.3)
  , NS "network" "alacritty --class network -e nmtui"
      (appName =? "network")
      (customFloating $ W.RationalRect 0.3 0.14 0.5 0.7)
  , NS "insomnia" "flatpak run rest.insomnia.Insomnia" (className =? "Insomnia")
      (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
  ]

