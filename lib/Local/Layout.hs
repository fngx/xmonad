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

inner = SG.group (spacing 0 2 $ Row.row Row.V) tabs [Just 1]
  where inner0 = tabs ||| acc ||| irow ||| icol
        as x = renamed [Replace x]
        il n = (as n) . (spacing 0 2)

        acc  = il "A" $ Accordion
        irow = il "R" $ Row.row Row.V
        icol = il "C" $ Row.row Row.H
        tabs = as "T" $ tabbed shrinkText Theme.decorations

wmii = SG.group outer inner [Just 1, Nothing]
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
  [ ("M-n", ("down", windows W.focusDown))
  , ("M-p", ("up", windows W.focusUp))

  , ("M-S-n", ("swap down", windows W.swapDown))
  , ("M-S-p", ("swap up", windows W.swapUp))

  -- delete column
  , ("M-s", ("make column", sendMessage $ SG.ChangeCapacities $ \z -> (Just 1):(W.integrate' z)))
  , ("M-v", ("make row", do sendMessage $ SG.ToCurrent $ SomeMessage $ SG.ChangeCapacities $ \z -> (Just 1):(W.integrate' z)
                            sendMessage $ SG.ChangeCapacities $ \z -> W.integrate' $ flip mapZ z $ \isF n -> if isF then Nothing else n
            ))

  , ("M--", ("shrink H",   outerRowMsg inner Row.Shrink))
  , ("M-=", ("grow H",     outerRowMsg inner Row.Grow))
  , ("M-S--", ("shrink V", innerRowMsg Row.Shrink))
  , ("M-S-=", ("grow V",   innerRowMsg Row.Grow))

  , ("M-'", ("reset",      do sendMessage $ SG.ToInner $ SomeMessage $ (Row.Equalize :: Row.Msg Window)
                              outerRowMsg inner Row.Equalize))

  , ("M-f", ("full", sendMessage $ Toggle FULL))
  , ("M-S-f", ("gfull", sendMessage $ SG.ToOuter $ SomeMessage $ NextLayout))

  , ("M-c M-c", ("normal", jump2 "C" "R"))
  , ("M-c M-r", ("flipped", jump2 "R" "C"))
  , ("M-c M-t", ("all tabs", jump2 "C" "T"))

  , ("M-l",   ("switchl",
               repeatHintedKeys
                [("M-l", ("cycle", cycleInnerLayout >> refresh))
                ,("M-t", ("tabs", jumpi "T"))
                ,("M-a", ("accordion", jumpi "A"))
                ,("M-r", ("rows", jumpi "R"))
                ,("M-c", ("cols", jumpi "C"))]
              ))

  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >> spawn "pkill -STOP xmobar" >> refresh))
  , ("M-b",   ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >> spawn "pkill -CONT xmobar" >> refresh))

  , ("M-k", ("kill", kill))

  ]
  where
    jump2 o i = do sendMessage $ SG.ToInner $ SomeMessage $ JumpToLayout i
                   sendMessage $ SG.ToOuter $ SomeMessage $ JumpToLayout o
    jumpi i = sendMessage $ SG.ToCurrent $ SomeMessage $ JumpToLayout i
    cycleInnerLayout = sendMessage $ SG.ToCurrent $ SomeMessage $ NextLayout

    outerRowMsg :: Typeable l => l Window -> Row.Msg (SG.Group l Window) -> X ()
    outerRowMsg _ m = sendMessage $ SG.ToOuter $ SomeMessage $ m

    innerRowMsg :: Row.Msg Window -> X ()
    innerRowMsg m = sendMessage $ SG.ToCurrent $ SomeMessage $ m
