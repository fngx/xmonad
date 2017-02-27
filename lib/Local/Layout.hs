{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (addLayout, layoutKeys) where

import Local.Windows (recentWindows)
import qualified Local.Theme as Theme

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Layout.Tabbed
import XMonad.StackSet (Workspace (Workspace), Stack (..))
import XMonad.Layout.TrackFloating
import XMonad.Layout.NoBorders
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), SetStruts (SetStruts))
import XMonad.Layout.LayoutCombinators
import XMonad.Actions.CycleWindows
import Local.MC

tabs = tabbed shrinkText Theme.decorations

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         mc tabs [(1, [1]), (1, [1])]

addLayout c =
  c { layoutHook = layout }

layoutKeys =
  let col n = (1, take n $ repeat 1)
      sendMC :: MCMsg Window -> X ()
      sendMC = sendMessage

      equalize a = zip (repeat 1) $ map ((map (const 1)) . snd) a
  in
  [ ("M-n", ("down", windows W.focusDown))
  , ("M-p", ("up",   windows W.focusUp))

  , ("M-m", ("focus master",  windows W.focusMaster))
  , ("M-S-m", ("swap master", windows W.swapMaster))

  , ("M-l 1", ("1",   sendMC $ SetCells [col 1] ))
  , ("M-l 2", ("1|1", sendMC $ SetCells [col 1, col 1] ))
  , ("M-l 3", ("1|2", sendMC $ SetCells [col 1, col 2] ))
  , ("M-l 4", ("1|3", sendMC $ SetCells [col 1, col 3] ))
  , ("M-l 5", ("2|2", sendMC $ SetCells [col 2, col 2] ))
  , ("M-l e", ("equalize", sendMC $ ChangeCells equalize))

  , ("M-=", ("grow", withFocused $ (sendMC . (ResizeCell 0.2 0.2))))
  , ("M--", ("shrink", withFocused $ (sendMC . (ResizeCell (-0.2) (-0.2)))))

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
