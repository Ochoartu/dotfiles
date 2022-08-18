import XMonad
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.IndependentScreens

import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Loggers

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

import XMonad.Layout.IndependentScreens

main :: IO ()
main = do 
       xmonad 
     . docks
     . ewmhFullscreen
     . ewmh
     . withSB ( xmobar0 <> xmobar1 <> xmobar2)  
     $ myConfig

myConfig = def
    { workspaces =  ["un","deux","trois","quatre","cinq","six"]
    , modMask = mod1Mask
    , layoutHook = myLayout
    , terminal = "kitty"
    , borderWidth = 8
    , normalBorderColor = "#e28743"
    , focusedBorderColor = "#1e81b0"
    , logHook = dynamicLogString ppThree >>= xmonadPropLog
    }
   `additionalKeysP`
    [ ("<XF86AudioMute>",  spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>",  spawn "pactl -- set-sink-volume 0 -05%")
    , ("<XF86AudioRaiseVolume>",  spawn "pactl -- set-sink-volume 0 +05%")
    , ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
    , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")
    ]

xmobar0 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0" (pure ppOne) 
xmobar1 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1" (pure ppTwo) 
xmobar2 = statusBarPropTo "_XMONAD_LOG_3" "xmobar -x 2" (pure ppThree) 

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom -b &"
    spawnOnce "nitrogen --restore"


myWorkspaces = ["1","2","3","4","5","6"]

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes



ppOne :: PP
ppOne = def

ppTwo :: PP
ppTwo = def

ppThree :: PP
ppThree = def
