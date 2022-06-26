import XMonad
import XMonad.Util.EZConfig
import System.Exit
import qualified XMonad.StackSet as W
import Data.Monoid
import qualified Data.Map as M
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.Navigation2D
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Util.Cursor
import XMonad.Util.SpawnOnce
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
  `additionalKeysP` (
    [("M-t",         spawn $ myTerminal)
    ,("M-!",         spawn "rofi -show drun")
    ,("M-w",         spawn "rofi -show window")
    ,("M-S-l",       spawn "lock.sh")
    ,("M-q",         spawn ("session.sh" ++ dmenuArgs))
    ,("M-C-q",       spawn "xmonad --recompile; xmonad --restart")
    ,("M-C-r",       refresh)
    ,("M-d",         spawn ("set_dp.sh" ++ dmenuArgs))
    ,("M-v",         spawn ("clipmenu -b -i -p '◧'" ++ dmenuArgs))
    ,("M-p",         spawn "restart_picom.sh")
    ,("M-*",         spawn "pkill -USR1 redshift")
    ,("Print",       spawn "screenshot.sh")
    ,("M-c",         spawn "clipshot.sh")
    ,("M-x",         kill)
    ,("M-<Space>",   sendMessage NextLayout)
    ,("M-j",         windows W.focusDown)
    ,("M-k",         windows W.focusUp)
    ,("M-m",         windows W.focusMaster)
    ,("M-<Return>",  windows W.swapMaster)
    ,("M-S-j",       windows W.swapDown)
    ,("M-S-k",       windows W.swapUp)
    ,("M-s",         toggleSmartSpacing)
    ,("M-f",         withFocused $ windows . W.sink)
    ,("M-<Tab>",     cycleRecentNonEmptyWS [xK_Super_L, xK_Super_R] xK_Tab xK_a)
    ,("M-h",         sendMessage Shrink)
    ,("M-l",         sendMessage Expand)
    ,("M-;",         sendMessage (IncMasterN 1))
    ,("M-,",         sendMessage (IncMasterN (-1)))
    ,("M-<Page_Down>",   prevWS)
    ,("M-<Page_Up>",     nextWS)
    ,("M-S-<Page_Down>", shiftToPrev)
    ,("M-S-<Page_Up>",   shiftToNext)
    ,("<XF86MonBrightnessUp>",   spawn "pral.sh light_up")
    ,("<XF86MonBrightnessDown>", spawn "pral.sh light_down")
    ,("<XF86AudioRaiseVolume>",  spawn "pral.sh sink_up")
    ,("<XF86AudioLowerVolume>",  spawn "pral.sh sink_down")
    ,("<XF86AudioMute>",         spawn "pral.sh sink_mute")
    ,("<XF86AudioMicMute>",      spawn "pral.sh source_mute")]
    ++
    [(m ++ "M-" ++ [k], f i)
        | (i, k) <- zip myWorkspaces workspaceKeys
        , (f, m) <- [(windows . W.greedyView, ""), (windows . W.shift, "S-")]]
    ++
    [(m ++ "M-" ++ [k], screenWorkspace sc >>= flip whenJust f)
        | (k, sc) <- zip screenKeys [0..]
        , (f, m) <- [(windows . W.view, ""), (windows . W.shift, "S-")]]
  )
  `additionalMouseBindings`
    [((modm, button1), \w -> focus w >> mouseMoveWindow w)
     , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
     , ((modm, button3), \w -> focus w >> mouseResizeWindow w)]

nav2DConfig = def {layoutNavigation = [("→", sideNavigation)
                                     , ("↓", sideNavigation)]}
nav2D = navigation2DP def ("<Up>", "<Left>", "<Down>", "<Right>")
                          [("M-", windowGo), ("M-S-", windowSwap)]
                          True

modm = mod4Mask
myTerminal = "alacritty"
myWorkspaces = ["1", "2", "3", "a", "z", "e", "4", "r", "5"]
workspaceKeys = "&é\"aze'r%"
screenKeys = "[]"

grey = "#404040"
darkGrey = "#262626"
stone = "#8c8c8c"

dmenuFn = "'JetBrains Mono:pixelsize=22:antialias=true'"
dDarkGrey = "\\#262626"
dNearBlack = "\\#0d0d0d"
dStone = "\\#8c8c8c"

dmenuArgs :: String
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
               <+> composeAll
    [ className =? "Gimp"               --> doFloat
    , className =? "jetbrains-toolbox"  --> doCenterFloat
    , className =? "feh"                --> doFloat
    , className =? "Galculator"         --> doCenterFloat
    , className =? "TeamSpeak 3"        --> doCenterFloat
    , className =? "Insomnia"           --> doCenterFloat
    , title     =? "win0"               --> doCenterFloat
    , title     =? "Contrôle du volume" --> doCenterFloat
    , resource  =? "desktop_window"     --> doIgnore ]

myEventHook = Hacks.trayerAboveXmobarEventHook <> Hacks.windowedFullscreenFixEventHook

myLogHook = return ()

myStartupHook = do
    setDefaultCursor xC_left_ptr -- set default cursor
    spawnOnce "picom --config /home/pierre/.config/picom.conf -b"
    spawnOnce "redshift -c /home/pierre/.config/redshift/redshift.conf"
    spawnOnce "dunst -c /home/pierre/.config/dunst/dunstrc"
    spawnOnce "udiskie"
    -- spawnOnce "bato"
    spawnOnce "clipmenud"
    spawnOnce "trayer -l --align left --distancefrom left --distance 420 --monitor primary --widthtype request --height 28 --transparent true --alpha 0 --tint 0x262626 --expand true --iconspacing 4 --SetPartialStrut true --SetDockType true"

bar :: PP
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

