import XMonad 
import qualified XMonad.StackSet as F 
import System.IO
import Graphics.X11.ExtraTypes.XF86
import XMonad.Layout.IndependentScreens
import qualified Data.Map as M
import XMonad.Util.EZConfig (additionalKeysP, mkNamedKeymap)
import XMonad.Util.Ungrab
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedActions

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
     . addDescrKeys ((mod4Mask, xK_F1), xMessage) myKeys
     $ myConfig

myConfig = def
    { 
      modMask = mod1Mask
    , layoutHook = spacingWithEdge 10  myLayout
    , terminal = myTerminal 
    , borderWidth = 6
    , workspaces = myWorkspaces
    , normalBorderColor = "#800080"
    , focusedBorderColor = "#FF00FF"
    , logHook = dynamicLogString ppThree >>= xmonadPropLog
    , startupHook = myStartupHook
    , manageHook = namedScratchpadManageHook scratchpads
    }

xmobar0 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0" (pure ppOne) 
xmobar1 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1" (pure ppTwo) 
xmobar2 = statusBarPropTo "_XMONAD_LOG_3" "xmobar -x 2" (pure ppThree) 

myStartupHook :: X ()
myStartupHook = do
    spawnOnce "picom -b &"
    spawnOnce "nitrogen --restore"
    spawnOnce "/usr/bin/emacs --daemon"

myWorkspaces = ["un","deux","trois","quatre","cinq","six"]
 

myLayout = avoidStruts (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    ratio   = 1/2    -- Default proportion of screen occupied by master pane
    delta   = 3/100  -- Percent of screen to increment by when resizing panes

myTerminal :: String
myTerminal = "kitty"

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

scratchpads :: NamedScratchpads
scratchpads = [ NS "terminal" spawnTerm findTerm manageTerm
              , NS "arandr"  spawnArandr findArandr manageArandr 
	      , NS "nitrogen" spawnNitrogen findNitrogen manageNitrogen
	      , NS "calculator" spawnCalc findCalc manageCalc
	      , NS "music" spawnMusic findMusic manageMusic]
  where
  spawnTerm = "st" ++ " -n scratchpad"
  findTerm  = resource =? "scratchpad"
  manageTerm = customFloating $ F.RationalRect l t w h
             where
	     h = 0.5
	     w = 0.4
	     t = 0.75 -h
	     l = 0.70 -w

  spawnArandr = "arandr"
  findArandr = className =? "Arandr"
  manageArandr = customFloating $ F.RationalRect l t w h
             where
	     h = 0.5
	     w = 0.4
	     t = 0.75 -h
	     l = 0.70 -w

  spawnNitrogen = "nitrogen"
  findNitrogen = className =? "Nitrogen"
  manageNitrogen = customFloating $ F.RationalRect l t w h
             where 
	     h = 0.5
	     w = 0.4
	     t = 0.75 -h
	     l = 0.70 -w

  spawnCalc = "qalculate-gtk"
  findCalc = className =? "Qalculate-gtk"
  manageCalc = customFloating $ F.RationalRect l t w h
             where
	     h = 0.5
	     w = 0.4
	     t = 0.75 -h
	     l = 0.70 -w

  spawnMusic = "cider"
  findMusic = className =? "Cider"
  manageMusic = customFloating $ F.RationalRect l t w h
             where
	     h = 0.5
	     w = 0.4
	     t = 0.75 -h
	     l = 0.70 -w

myKeys :: XConfig l0 -> [((KeyMask,KeySym), NamedAction)]
myKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
     [ ("<XF86AudioMute>", addName "Silence" $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
     , ("<XF86AudioLowerVolume>", addName "Lower Volume" $  spawn "pactl -- set-sink-volume 0 -05%")
     , ("<XF86AudioRaiseVolume>", addName "Raise Volume" $ spawn "pactl -- set-sink-volume 0 +05%")
     , ("<XF86MonBrightnessUp>", addName "Light it up" $ spawn "lux -a 10%")
     , ("<XF86MonBrightnessDown>", addName "Light it down" $ spawn "lux -s 10%")
     , ("M-C-o", addName "Toggle scratchpad Terminal" $ namedScratchpadAction scratchpads "terminal")
     , ("M-C-l", addName "Toggle scratchpad Arandr" $ namedScratchpadAction scratchpads "arandr")
     , ("M-C-p", addName "Wallpaper" $ namedScratchpadAction scratchpads "nitrogen")
     , ("M-C-i", addName "Calculator" $ namedScratchpadAction scratchpads "calculator")
     , ("M-C-m", addName "Music" $ namedScratchpadAction scratchpads "music")
     ]

