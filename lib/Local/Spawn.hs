module Local.Spawn where

import XMonad

spawnKeys :: [ (String, X ()) ]
spawnKeys =
  [ ("M-<Return>", spawn "emacsclient -c -n -e '(multi-term-quick-frame)' 2>&1 > /dev/null")
  , ("M-S-<Return>", spawn "emacsclient -c -n 2>&1 > /dev/null")

  , ("<XF86MonBrightnessUp>", spawn "xbacklight -steps 0 -5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -steps 0 +5")
  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
  , ("<XF86AudioMute>", spawn "pamixer -t")

  , ("M-q h", spawn "systemctl hibernate")
  , ("M-q s", spawn "systemctl suspend")
  , ("M-q M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

  , ("M-a c", spawn "chromium 2>&1 > /dev/null")
  , ("M-a t", spawn "urxvt 2>&1 > /dev/null")
  , ("M-a w", spawn "conkeror 2>&1 > /dev/null")
  ]
