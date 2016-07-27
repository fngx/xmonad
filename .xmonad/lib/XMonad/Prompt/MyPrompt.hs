module XMonad.Prompt.MyPrompt where

import XMonad (spawn)
import XMonad.Prompt
import XMonad.Prompt.Shell
import Data.Maybe (fromMaybe)

pconfig = def
  { position = Top
  , font = "xft:Monospace-10"
  , height = 24
  , borderColor = "#222"
  }

qconfig = pconfig
  { autoComplete = Just 1000
  , alwaysHighlight = True
  }

shell = shellPrompt pconfig

data Quick = Quick

instance XPrompt Quick where
  showXPrompt Quick = "Act: "

quick =
  let commands = [ ("emacs", spawn "emacsclient -c -n")
                 , ("qb", spawn "qb")
                 -- todo read zsh marks file
                 , ("hibernate", spawn "systemctl hibernate")
                 , ("suspend", spawn "systemctl suspend")
                 , ("poweroff", spawn "systemctl poweroff")
                 ]
  in mkXPrompt Quick qconfig (mkComplFunFromList' (map fst commands)) $
     fromMaybe (return ()) . (`lookup` commands)
