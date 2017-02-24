{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (addLayout, layoutKeys) where

import Local.Windows (recentWindows)
import qualified Local.Theme as Theme

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Layout.Groups.Examples
import XMonad.Layout.ZoomRow
import XMonad.Layout.Tabbed
import XMonad.StackSet (Workspace (Workspace), Stack (..))
import XMonad.Util.Stack
import XMonad.Layout.TrackFloating
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.MessageFeedback
import Control.Monad (unless, when)
import qualified Local.Row as Row
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), SetStruts (SetStruts))
import XMonad.Layout.Renamed
import Data.List (find)
import Data.Monoid
import System.IO
import Data.Maybe
import XMonad.Layout.LayoutCombinators
import XMonad.Util.Stack
import Data.IORef
import XMonad.Actions.CycleWindows
import Local.MC


tabs = as "t" $ tabbed shrinkText Theme.decorations
  where as x = renamed [Replace x]


layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         MC { cells = [(1, [1]), (1, [1,1])]
            , overflow = tabs }

addLayout c =
  c { layoutHook = layout }

layoutKeys =
  let col n = (1, take n $ repeat 1) in
  [ ("M-n", ("down", windows W.focusDown))
  , ("M-p", ("up",   windows W.focusUp))

  , ("M-m", ("focus master",  windows W.focusMaster))
  , ("M-S-m", ("swap master", windows W.swapMaster))

  , ("M-l 1", ("1",   sendMessage $ SetCells [col 1] ))
  , ("M-l 2", ("1|1", sendMessage $ SetCells [col 1, col 1] ))
  , ("M-l 3", ("1|2", sendMessage $ SetCells [col 1, col 2] ))
  , ("M-l 4", ("1|3", sendMessage $ SetCells [col 1, col 3] ))
  , ("M-l 5", ("2|2", sendMessage $ SetCells [col 2, col 2] ))

  , ("M-M1-n", ("rfd", rotUnfocusedDown))
  , ("M-M1-p", ("rfd", rotUnfocusedUp))

  , ("M-S-n", ("swap down", windows W.swapDown))
  , ("M-S-p", ("swap up", windows W.swapUp))

  , ("M-f", ("full", sendMessage $ Toggle FULL))

  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >>
                          spawn "pkill -STOP xmobar" >> refresh))
  , ("M-b",   ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >>
                           spawn "pkill -CONT xmobar" >> refresh))

  , ("M-k", ("kill", kill))]
