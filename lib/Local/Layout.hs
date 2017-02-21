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
import Local.Spacing (spacing)
import XMonad.Layout.Accordion
import Local.Hints (repeatHintedKeys)
import qualified Local.SimpleGroups as SG
import XMonad.Util.Stack
import Data.IORef
import XMonad.Actions.CycleWindows

inner = SG.group (spacing 0 2 $ Row.row Row.V) tabs [1]
  where as x = renamed [Replace x]
        tabs = as "t" $ tabbed shrinkText Theme.decorations

-- could I specialcase SG.group so that
-- it knows about the capacity of an outer group
-- because it looks at the inner group

wmii = SG.group outer inner [1, 1]
  where outer = spacing 4 2 $ ocol ||| Full ||| orow
        ocol = Row.row Row.H
        orow = Row.row Row.V

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         wmii

addLayout c =
  c { layoutHook = layout }

layoutKeys =
  let jump2 o i = do sendMessage $ SG.ToInner $ SomeMessage $ JumpToLayout i
                     sendMessage $ SG.ToOuter $ SomeMessage $ JumpToLayout o
      jumpi i = sendMessage $ SG.ToCurrent $ SomeMessage $ JumpToLayout i
      cycleInnerLayout = sendMessage $ SG.ToCurrent $ SomeMessage $ NextLayout

      outerRowMsg :: Typeable l => l Window -> Row.Msg (SG.Group l Window) -> X ()
      outerRowMsg _ m = sendMessage $ SG.ToOuter $ SomeMessage $ m

      innerRowMsg :: Row.Msg Window -> X ()
      innerRowMsg m = sendMessage $ SG.ToCurrent $ SomeMessage $ m

      updateColumnCapacity = do
        ref <- io $ newIORef Nothing
        sendMessage $ SG.ToCurrent $ SomeMessage $ SG.GetCapacities ref
        rowCaps <- io $ readIORef ref
        sendMessage $ SG.ChangeCapacity $ const $ sum $ W.integrate' rowCaps
  in
  [ ("M-n", ("down", windows W.focusDown))
  , ("M-p", ("up", windows W.focusUp))

  , ("M-M1-n", ("rfd", rotFocusedDown))
  , ("M-M1-p", ("rfd", rotFocusedUp))

  , ("M-,", ("new row", do sendMessage $ SG.ToCurrent $ SomeMessage $ SG.AddGroup
                           updateColumnCapacity))

  , ("M-.", ("del row", do sendMessage $ SG.ToCurrent $ SomeMessage $ SG.DeleteGroup
                           updateColumnCapacity))

  , ("M-S-,", ("new col", sendMessage $ SG.AddGroup))
  , ("M-S-.", ("del col", do sendMessage $ SG.DeleteGroup
                             updateColumnCapacity))

  , ("M-c M-t", ("new tab", do sendMessage $ SG.ToCurrent $ SomeMessage $ SG.ChangeCapacity (+ 1)
                               updateColumnCapacity))

  , ("M-S-n", ("swap down", windows W.swapDown))
  , ("M-S-p", ("swap up", windows W.swapUp))

  , ("M--", ("shrink H",   outerRowMsg inner Row.Shrink))
  , ("M-=", ("grow H",     outerRowMsg inner Row.Grow))
  , ("M-S--", ("shrink V", innerRowMsg Row.Shrink))
  , ("M-S-=", ("grow V",   innerRowMsg Row.Grow))

  , ("M-'", ("reset",      do sendMessage $ SG.ToInner $ SomeMessage $ (Row.Equalize :: Row.Msg Window)
                              outerRowMsg inner Row.Equalize))

  , ("M-f", ("full", sendMessage $ Toggle FULL))
  , ("M-S-f", ("gfull", sendMessage $ SG.ToOuter $ SomeMessage $ NextLayout))

  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >> spawn "pkill -STOP xmobar" >> refresh))
  , ("M-b",   ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >> spawn "pkill -CONT xmobar" >> refresh))

  , ("M-k", ("kill", kill))]
