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

import XMonad.Layout.Spacing

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
    , layoutHook = spacingWithEdge 10 $ myLayout
    , terminal = "kitty"
    , borderWidth = 6 
    , normalBorderColor = "#800080"
    , focusedBorderColor = "#FF00FF"
    , logHook = dynamicLogString ppThree >>= xmonadPropLog
    , startupHook = myStartupHook
    }
   `additionalKeysP`
    [ ("<XF86AudioMute>",  spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
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
  { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
