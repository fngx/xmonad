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

tabs = as "t" $ tabbed shrinkText Theme.decorations
  where as x = renamed [Replace x]

outer = spacing 4 2 $ ocol
  where ocol = Row.row Row.H

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         SG.group outer inner [1, 1]

addLayout c =
  c { layoutHook = layout }

layoutKeys =
  let rowMsg :: Typeable l => l Window -> Row.Msg (SG.Group l Window) -> SomeMessage
      rowMsg _ m = SomeMessage $ m

      flip = do sendMessage $ SG.ToInner $ SomeMessage $ SG.ToOuter $ rowMsg tabs Row.Flip
                sendMessage $ SG.ToOuter $ rowMsg inner Row.Flip

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

  , ("M-v", ("flip", flip))

  , ("M-S-,", ("new col", sendMessage $ SG.AddGroup))
  , ("M-S-.", ("del col", do sendMessage $ SG.DeleteGroup
                             updateColumnCapacity))

  , ("M-c M-t", ("new tab", do sendMessage $ SG.ToCurrent $ SomeMessage $ SG.ChangeCapacity (+ 1)
                               updateColumnCapacity))

  , ("M-S-n", ("swap down", windows W.swapDown))
  , ("M-S-p", ("swap up", windows W.swapUp))

  , ("M--", ("shrink H",   sendMessage $ SG.ToOuter   $ rowMsg inner Row.Shrink))
  , ("M-=", ("grow H",     sendMessage $ SG.ToOuter   $ rowMsg inner Row.Grow))
  , ("M-S--", ("shrink V", sendMessage $ SG.ToCurrent $ rowMsg tabs Row.Shrink))
  , ("M-S-=", ("grow V",   sendMessage $ SG.ToCurrent $ rowMsg tabs Row.Grow))

  , ("M-'", ("reset",      do sendMessage $ SG.ToOuter $ rowMsg inner Row.Equalize
                              sendMessage $ SG.ToInner $ rowMsg tabs Row.Equalize))

  , ("M-f", ("full", sendMessage $ Toggle FULL))

  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >> spawn "pkill -STOP xmobar" >> refresh))
  , ("M-b",   ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >> spawn "pkill -CONT xmobar" >> refresh))

  , ("M-k", ("kill", kill))]
