import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageHelpers     ( doCenterFloat
                                                , doFullFloat
                                                , isFullscreen
                                                )
import           XMonad.ManageHook
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.SpawnOnce

import           XMonad.Layout.CenteredMaster
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.ToggleLayouts   as T

import qualified XMonad.StackSet               as W

import           GHC.IO.Exception               ( ExitCode(ExitSuccess) )
import           System.Exit                    ( exitSuccess )


myTerminal = "alacritty"
myBar = "xmobar ~/.xmonad/.xmobarrc"
myTray = "trayer"
myTrayOptions =
  "--edge bottom --align right --SetDockType true --SetPartialStrut true \
  \--expand true --transparent true --tint 0x333333 --alpha 0 &"
myBrowser = "brave"
myModMask = mod4Mask -- Win key or Super_L
myBorderWidth = 4
myNormalBorderColor = "#1d2021"
myFocusedBorderColor = "#98971a"
myAppLauncher =
  "rofi -theme gruvbox-dark-hard -lines 12 -padding 18 -width 60 -location 0 -show drun -sidebar-mode -columns 3 -font 'Noto Sans 12'"
myMenu =
  "dmenu_run -nf '#fbf1c7' -sf '#282828' -sb '#98971a' -fn 'DejaVu Sans Mono:size=10'"

-- Scratchpads
scratchpads :: [NamedScratchpad]
scratchpads = [tScratch "htop", tScratch "pulsemixer"]
 where
  tCmd app = myTerminal ++ " --class '" ++ app ++ "' -e " ++ app
  tScratch cmd = NS cmd (tCmd cmd) (resource =? cmd) defaultFloating

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP :: PP
myPP =
  xmobarPP { ppCurrent = xmobarColor myFocusedBorderColor "" . wrap "[" "]" }

myManageHook =
  composeAll
      [ appName =? "pulsemixer" --> doCenterFloat
      , appName =? "htop" --> doCenterFloat
      , isFullscreen --> doFullFloat
      ]
    <+> namedScratchpadManageHook scratchpads

-- Keybinds
myKeys =
  -- App shortcuts
  [ ("M-b"        , spawn myBrowser)
  , ("M1-b", spawn $ myBrowser ++ " --incognito")
  , ("M1-s"       , spawn "slack")
  , ("M1-e"       , spawn "element-desktop")
  , ("M-<Return>" , spawn myTerminal)
  , ("<Print>"    , spawn "flameshot gui")
  -- Layout and Window controls
  , ("M1-<Return>", windows W.swapMaster)
  , ("M-f"        , sendMessage T.ToggleLayout)
  , ("M-q"        , kill)
  , ("M-S-t", namedScratchpadAction scratchpads "htop")
  , ("M-S-p", namedScratchpadAction scratchpads "pulsemixer")
  -- Runner shortcuts
  , ("M1-p"       , spawn myAppLauncher)
  , ("M-p"        , spawn myMenu)
  -- Restart xmonad
  , ( "M-r"
    , spawn
      "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
    )
  -- Audio Controls
  , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ( "<XF86AudioLowerVolume>"
    , spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%"
    )
  , ( "<XF86AudioRaiseVolume>"
    , spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%"
    )
  -- Brightness controls
  , ("<F1>", spawn "brightnessctl s 1%")
  , ("<F2>", spawn "brightnessctl s 10%-")
  , ("<F3>", spawn "brightnessctl s 10%+")
  ]

-- Autostarts startup :: X ()
myStartup = do
  spawn ("killall " ++ myTray)
  spawn ("sleep 2 && " ++ myTray ++ " " ++ myTrayOptions)
  spawnOnce "pasystray"
  spawnOnce "nm-applet"
  spawnOnce "pamac-tray"
  spawnOnce "xfce4-power-manager"
  spawnOnce "picom --experimental-backends --corner-radius 8 -o 0.0 -b"
  spawnOnce "~/.fehbg"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig { XMonad.modMask = modMask } = (mod1Mask, xK_f)

-- Layout
myLayout = toggledTiled ||| toggledMirror ||| T.toggleLayouts
  (topRightMaster tiled)
  (centerMaster tiled)
 where
     -- default tiling algorithm partitions the screen into two panes
  tiled = smartSpacingWithEdge 4 $ smartBorders $ Tall nmaster delta ratio

  full          = noBorders Full

  mirror        = Mirror tiled

  toggledTiled  = T.toggleLayouts full tiled

  toggledMirror = T.toggleLayouts full mirror

  -- The default number of windows in the master pane
  nmaster       = 1

  -- Default proportion of screen occupied by master pane
  ratio         = 2 / 3

  -- Percent of screen to increment by when resizing panes
  delta         = 3 / 100


-- Main configuration
myConfig =
  def { terminal           = myTerminal
      , modMask            = myModMask
      , borderWidth        = myBorderWidth
      , manageHook         = myManageHook
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor
      , handleEventHook    = fullscreenEventHook
      , startupHook        = myStartup
      , layoutHook         = myLayout
      }
    `additionalKeysP` myKeys


main :: IO ()
main = do
  xmonad . ewmh =<< statusBar myBar myPP toggleStrutsKey myConfig
