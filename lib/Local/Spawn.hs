module Local.Spawn where

import XMonad

spawnKeys :: [ (String, X ()) ]
spawnKeys =
  [ ("M-<Return>", spawn "emacsclient -c -n -e '(multi-term-quick-frame)'")
  , ("M-S-<Return>", spawn "emacsclient -c -n")
  , ("M-i", spawn "notify-send \"$(date '+%R %a %b %d')\" \"$(acpi)\"")

  , ("<XF86MonBrightnessUp>", spawn "xbacklight -steps 0 -5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -steps 0 +5")
  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
  , ("<XF86AudioMute>", spawn "pamixer -t")

  , ("M-s h", spawn "systemctl hibernate")
  , ("M-s s", spawn "systemctl suspend")

  , ("M-a c", spawn "chromium")
  , ("M-a t", spawn "urxvt")
  , ("M-a w", spawn "conkeror")
  ]
