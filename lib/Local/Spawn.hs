module Local.Spawn where

import XMonad

spawnKeys :: [ (String, X ()) ]
spawnKeys =
  [ ("M-<Return>", spawn "urxvt")
  , ("M-S-<Return>", spawn "emacsclient -c -n")
  , ("M-i", spawn "notify-send \"$(date '+%R %a %b %d')\" \"$(acpi)\"")

  , ("<XF86MonBrightnessUp>", spawn "xbacklight -steps 0 -5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -steps 0 +5")
  , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 5")
  , ("<XF86AudioLowerVolume>", spawn "pamixer -d 5")
  , ("<XF86AudioMute>", spawn "pamixer -t")

  -- TODO key/script to reset xrandr


  , ("M-s h", spawn "systemctl hibernate")
  ]
