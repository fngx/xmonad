module XMonad.Prompt.MyPrompt where

import XMonad (spawn)
import XMonad.Prompt
import XMonad.Prompt.Shell
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)

pconfig = def
  { position = Top
  , font = "xft:Monospace-10"
  , height = 24
  , borderColor = "#222"
  , searchPredicate = \i c -> i `isInfixOf` c
  }

qconfig = pconfig
  { autoComplete = Just 5000
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
  in mkXPrompt Quick qconfig (\s -> return $ filter (searchPredicate qconfig $ s) (map fst commands)) $
     fromMaybe (return ()) . (`lookup` commands)
