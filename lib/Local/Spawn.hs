module Local.Spawn where

import XMonad

reloadCommand = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

spawnKeys :: [ (String, (String, X ())) ]
spawnKeys =
  [ ("M-<Return>", ("term", spawn "urxvt"-- "emacsclient -c -n -e '(multi-term-quick-frame)' 2>&1 > /dev/null"
    ))
  , ("M-S-<Return>", ("emacs", spawn "emacsclient -c -n 2>&1 > /dev/null"))

  , ("<XF86MonBrightnessUp>", ("brighter", spawn "xbacklight -steps 0 -5"))
  , ("<XF86MonBrightnessDown>", ("darker", spawn "xbacklight -steps 0 +5"))
  , ("<XF86AudioRaiseVolume>", ("louder", spawn "pamixer -i 5"))
  , ("<XF86AudioLowerVolume>", ("quieter", spawn "pamixer -d 5"))
  , ("<XF86AudioMute>", ("mute", spawn "pamixer -t"))
  , ("M-q h", ("hibernate", spawn "systemctl hibernate"))
  , ("M-q s", ("suspend", spawn "systemctl suspend"))
  , ("M-q M-q", ("reload", spawn reloadCommand))

  , ("M-a c", ("chrome", spawn "chromium 2>&1 > /dev/null"))
  , ("M-a t", ("term", spawn "urxvt"))
  , ("M-a w", ("web", spawn "conkeror 2>&1 > /dev/null"))
  , ("M-a x", ("clock", spawn "xclock"))
  , ("M-a m", ("mail check", spawn "notmuch new"))
  ]
