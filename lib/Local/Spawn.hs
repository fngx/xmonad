module Local.Spawn where

import XMonad
import XMonad.Util.Ungrab (unGrab)
import XMonad.Util.Paste (pasteSelection)
import XMonad.Util.XSelection (safePromptSelection)
import XMonad.Actions.WindowGo
import XMonad.Actions.DynamicWorkspaces
import Data.List (isPrefixOf)

reloadCommand = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

spawnKeys :: [ (String, (String, X ())) ]
spawnKeys =
  let emacs = ("emacs", spawn "emacsclient -c -n 2>&1 > /dev/null")
      isNotmuch s
        | "*notmuch-search-" `isPrefixOf` s = True
        | "*notmuch-saved-search-" `isPrefixOf` s = True
        | otherwise = False

  in
  [ ("M-<Return>", ("term", spawn "urxvt"))
  , ("M-S-<Return>", emacs)

  , ("<XF86MonBrightnessUp>", ("brighter", spawn "xbacklight -inc 10"))
  , ("<XF86MonBrightnessDown>", ("darker", spawn "xbacklight -dec 10"))
  , ("<XF86AudioRaiseVolume>", ("louder", spawn "pamixer -i 5"))
  , ("<XF86AudioLowerVolume>", ("quieter", spawn "pamixer -d 5"))
  , ("<XF86AudioMute>", ("mute", spawn "pamixer -t"))

  , ("M-q h", ("hibernate", spawn "systemctl hibernate"))
  , ("M-q s", ("suspend", spawn "systemctl suspend"))
  , ("M-q M-q", ("reload", spawn reloadCommand))
  , ("M-q w", ("wifi", spawn "wpa_gui"))
  , ("M-q v v", ("vpn", spawn "vpn toggle"))
  , ("M-q v s", ("vpn st?", spawn "vpn notify"))

  , ("M-a c", ("chrome", spawn "chromium 2>&1 > /dev/null"))
  , ("M-a t", ("term", spawn "urxvt"))
  , ("M-a w", ("web", spawn "conkeror 2>&1 > /dev/null"))
  , ("M-a x", ("clock", spawn "xclock"))
  , ("M-a m", ("mail check", spawn "notmuch new"))
  , ("M-a e", emacs)

  , ("M-a g", ("telegram",
               raiseMaybe (do addWorkspace "mail"
                              spawn "telegram-desktop")
                (className =? "telegram-desktop")))
  , ("M-a i", ("inbox",
               raiseMaybe (do addWorkspace "mail"
                              spawn "emacsclient -n -c -e '(my-inbox)'")
                (className =? "Emacs" <&&> (fmap isNotmuch title))))
  , ("M-a a", ("agenda", raiseMaybe (spawn "emacsclient -e '(org-agenda nil \"a\")'")
                         (className =? "Emacs" <&&> title =? "*Org Agenda*")))

  , ("M-y", ("paste", unGrab >> spawn "xdotool type -clearmodifiers -- \"$(xclip -o -selection primary)\""))
  , ("M-S-y", ("open", safePromptSelection "xdg-open"))]
