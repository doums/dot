import XMonad
import XMonad.Util.EZConfig
import System.Exit
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Monoid
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.TopicSpace
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
  workspaces         = topicNames topicItems,
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

-- key bindings
keybinds = ([
  -- XMonad basics
    ("M-C-q",       spawn "xmonad --recompile; xmonad --restart")
  , ("M-C-r",       refresh)
  , ("M-x",         kill)
  -- Topic navigation
  , ("M-<Tab>",     switchToLastTopic)
  , ("M-<Page_Up>", nextWS)
  , ("M-<Page_Down>",
                    prevWS)
  -- Window navigation
  , ("M-k",         windows W.focusUp)
  , ("M-j",         windows W.focusDown)
  , ("M-m",         windows W.focusMaster)
  , ("M-<Return>",  windows W.swapMaster)
  , ("M-S-k",       windows W.swapUp)
  , ("M-S-j",       windows W.swapDown)
  , ("M-S-<Page_Up>",
                    shiftToNext)
  , ("M-S-<Page_Down>",
                    shiftToPrev)
  -- Layout
  , ("M-<Space>",   sendMessage NextLayout)
  , ("M-f",         withFocused $ windows . W.sink)
  , ("M-l",         sendMessage Expand)
  , ("M-h",         sendMessage Shrink)
  , ("M-;",         sendMessage (IncMasterN 1))
  , ("M-,",         sendMessage (IncMasterN (-1)))
  -- Launch app
  , ("M-t",         spawn $ myTerminal)
  , ("M-!",         spawn "rofi -show drun")
  , ("M-w",         spawn "rofi -show window")
  , ("M-S-l",       spawn "lock.sh")
  , ("M-q",         spawn ("session.sh" ++ dmenuArgs))
  , ("M-v",         spawn ("clipmenu -b -i -p '◧'" ++ dmenuArgs))
  , ("M-p",         spawn "restart_picom.sh")
  , ("M-*",         spawn "pkill -USR1 redshift")
  , ("Print",       spawn "screenshot.sh")
  , ("M-c",         spawn "clipshot.sh")
  -- Scratchpads
  , ("M-n", namedScratchpadAction scratchpads "gtrans")
  , ("M-o", namedScratchpadAction scratchpads "pavucontrol")
  , ("M-b", namedScratchpadAction scratchpads "filemanager")
  , ("M-=", namedScratchpadAction scratchpads "calc")
  , ("M-i", namedScratchpadAction scratchpads "irc")
  , ("M-s", namedScratchpadAction scratchpads "bottom")
  -- Multimedia
  , ("<XF86MonBrightnessUp>",   spawn "pral.sh light_up")
  , ("<XF86MonBrightnessDown>", spawn "pral.sh light_down")
  , ("<XF86AudioRaiseVolume>",  spawn "pral.sh sink_up")
  , ("<XF86AudioLowerVolume>",  spawn "pral.sh sink_down")
  , ("<XF86AudioMute>",         spawn "pral.sh sink_mute")
  , ("<XF86AudioMicMute>",      spawn "pral.sh source_mute") ]
  ++
  -- Basic topic navigation
  -- M-<123aze4r5> move to topic x
  -- M-S-<123aze4r5> move current window to topic x
  [ (m ++ "M-" ++ [k], f i)
      | (i, k) <- zip (topicNames topicItems) topicKeys
      , (f, m) <- [(goto, ""), (windows . W.shift, "S-")] ]
  ++
  -- Basic screen navigation
  -- M-<[]> move to next/previous screen
  -- M-S-<[]> move current window to next/previous screen
  [ (m ++ "M-" ++ [k], screenWorkspace sc >>= flip whenJust f)
      | (k, sc) <- zip screenKeys [0..]
      , (f, m) <- [(windows . W.view, ""), (windows . W.shift, "S-")] ]
  )

mousebinds = [
  ((modm, button1), \w -> focus w >> mouseMoveWindow w)
  , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
  , ((modm, button3), \w -> focus w >> mouseResizeWindow w) ]

nav2DConfig = def {layoutNavigation = [("→", sideNavigation)
                                     , ("↓", sideNavigation)]}
nav2D = navigation2DP def ("<Up>", "<Left>", "<Down>", "<Right>")
                          [("M-", windowGo), ("M-S-", windowSwap)]
                          True

modm = mod4Mask
myTerminal = "alacritty"
topicKeys = "&é\"aze'r%"
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
    , className =? "Galculator"         --> doCenterFloat
    , className =? "TeamSpeak 3"        --> doCenterFloat
    , className =? "Insomnia"           --> doCenterFloat
    , className =? "jetbrains-toolbox"  --> doCenterFloat
    , title =? "splash"
        <&&> className ^? "jetbrains-"  --> doCenterFloat <> hasBorder False
    , (className =? "firefox" <&&> resource =? "Dialog")
                                        --> doFloat
    , title     =? "Contrôle du volume" --> doCenterFloat
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

-- topics
-- 1›terminal 2›web browser 3›IDE a›keybase z›empty r›element 4›empty r›empty 5›empty
topicItems = 
  [ inHome   "\985015"                spawnShell
  , inHome   "\984479"                (spawn "firefox")
  , TI       "\987350"  "Documents"   (spawn "jetbrains-toolbox")
  , inHome   "\985977"                (spawn "run_keybase")
  , noAction "z"        "Documents"
  , inHome   "\984960"                (spawn "flatpak run im.riot.Riot")
  , noAction "4"          "~"
  , noAction "r"          "~"
  , noAction "5"          "~"
  ]

myTopicConfig = def
  { topicDirs          = tiDirs    topicItems
  , topicActions       = tiActions topicItems
  , defaultTopicAction = const (pure ())
  , defaultTopic       = "\985015" -- 1›terminal
  }

-- topics helper functions
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

spawnShellIn dir = spawn $ myTerminal ++ " --working-directory " ++ dir

goto = switchTopic myTopicConfig

switchToLastTopic = switchNthLastFocusedByScreen myTopicConfig 1

-- scratchpads
scratchpads =
  [ NS "gtrans" "alacritty --class gtrans -e trans -I -show-original no -show-languages no en:fr"
      (appName =? "gtrans")
      (customFloating $ W.RationalRect (1/4) (1/6) (3/9) (4/6))
  , NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol")
      (customFloating $ W.RationalRect (1/4) (1/4) (2/4) (2/4))
  , NS "filemanager" "alacritty --class filemanager -e nnn"
      (appName =? "filemanager")
      (customFloating $ W.RationalRect (1/4) (1/6) (3/9) (4/6))
  , NS "irc" "alacritty --class irc -e tiny"
      (appName =? "irc")
      (doFloatAt 0.2 0.3)
  , NS "bottom" "alacritty --class systemMonitor -e btm"
      (appName =? "systemMonitor")
      (customFloating $ W.RationalRect (1/6) (1/9) (4/6) (7/9))
  , NS "calc" "alacritty --class calc -e kalker"
      (appName =? "calc")
      (customFloating $ W.RationalRect (1/4) (2/6) (1/4) (3/6))
  ]

