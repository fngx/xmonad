module XMonad.Prompt.MyPrompt where

import XMonad (spawn)
import XMonad.Prompt
import XMonad.Prompt.Shell
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)

pconfig = def
  { position = Bottom
  , font = "xft:Monospace-10"
  , height = 24
  , borderColor = "#888888"
  , fgColor = "white"
  , searchPredicate = \i c -> i `isInfixOf` c
  }

qconfig = pconfig
  { autoComplete = Just 100000
  , alwaysHighlight = True
  }

shell = shellPrompt pconfig
