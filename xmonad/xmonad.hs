import XMonad
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
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "polybar" (pure myPolybarPP)) defToggleStrutsKey
     $ myConfig

myConfig = def
    { workspaces =  ["un","deux","trois","quatre","cinq","six"]
    , modMask = mod1Mask
    --, layoutHook = myLayout
    , terminal = "kitty"
    , borderWidth = 8
    , normalBorderColor = "#800080"
    , focusedBorderColor = "#FF00FF"
    }
   `additionalKeysP`
    [ ("<XF86AudioMute>",  spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>",  spawn "pactl -- set-sink-volume 0 -05%")
    , ("<XF86AudioRaiseVolume>",  spawn "pactl -- set-sink-volume 0 +05%")
    , ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
    , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")
    ]


myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom -b &"
    spawnOnce "nitrogen --restore"

myPolybarPP :: PP
myPolybarPP = def

myWorkspaces = ["1","2","3","4","5","6"]


