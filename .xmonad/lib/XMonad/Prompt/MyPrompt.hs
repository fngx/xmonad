module XMonad.Prompt.MyPrompt where

import XMonad (spawn)
import XMonad.Prompt
import XMonad.Prompt.Shell
import Data.Maybe (fromMaybe)
import Data.List (isInfixOf)
import qualified XMonad.Util.Colours as Cs

pconfig = def
  { position = Bottom
  , font = "xft:Monospace-10"
  , height = 24
  , borderColor = Cs.dimBorder
  , fgColor = Cs.text
  , bgColor = Cs.background
  , fgHLight = Cs.urgentText
  , bgHLight = Cs.urgent
  , searchPredicate = \i c -> i `isInfixOf` c
  }

qconfig = pconfig
  { autoComplete = Just 100000
  , alwaysHighlight = True
  }

shell = shellPrompt pconfig
