{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Rows where

import XMonad.Core ( SomeMessage(..) )
import XMonad (sendMessage, Window, ChangeLayout(NextLayout), X, WindowSet, windows)
--import XMonad.Layout (Full (Full))
import XMonad.StackSet (Stack (Stack))
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import XMonad.Layout.Row (row, orderRow, Axis (H, V), axis, isOuterLayout)
import XMonad.Layout.Tabbed (tabbed, shrinkText)
import XMonad.Layout.LayoutCombinators ( (|||) , JumpToLayout (JumpToLayout, Wrap) )
import XMonad.Layout.Decoration (def, fontName, decoHeight, activeBorderColor)

import qualified XMonad.Layout.Groups.Helpers as G
import XMonad.Layout.Groups
import Control.Monad (unless, when)
import qualified Data.Set as S
import XMonad.Actions.MessageFeedback (send)
import XMonad.Layout.LayoutModifier

myTheme = def
  { fontName = "xft:Monospace-8"
  , decoHeight = 16
  }

rows = let t = tabbed shrinkText myTheme
           inner = row V ||| t
           outer = orderRow H
       in group inner outer

data Balance a = Balance (S.Set a) deriving (Show, Read)

balance = ModifiedLayout (Balance S.empty)

instance (Ord a, Read a, Show a) => LayoutModifier Balance a where
  redoLayout (Balance ws) _ st rs = do
    let ws' = S.fromList $ W.integrate' st
    when (ws' /= ws) $ sendMessage $ Modify rebalance
    return $ (rs, Just $ Balance $ ws')

rebalance :: ModifySpec
rebalance l gs@(Just (Stack (G _ (Just (Stack _ (_:_) _))) [] [])) = moveToNewGroupDown l gs
rebalance l gs@(Just (Stack (G _ (Just (Stack _ _ (_:_)))) [] [])) = moveToNewGroupDown l gs
rebalance _ gs = gs

alt :: ModifySpec -> (WindowSet -> WindowSet) -> X ()
alt f g = alt2 (Modify f) $ windows g

alt2 :: GroupsMessage -> X () -> X ()
alt2 m x = do b <- send m
              unless b x

atFirstZ Nothing = True
atFirstZ (Just (Stack f u d)) = null u

atLastZ Nothing = True
atLastZ (Just (Stack f u d)) = null d

goFirstZ = until atFirstZ focusUpZ
goLastZ = until atLastZ focusDownZ

focusPrevZ :: ModifySpec
focusPrevZ l gs = let f = getFocusZ gs in
  case f of
    Nothing -> gs
    Just s -> if atFirstZ (gZipper s) then
                onFocused goLastZ l (focusGroupUp l gs)
              else
                focusUp l gs

focusNextZ :: ModifySpec
focusNextZ l gs = let f = getFocusZ gs in
  case f of
    Nothing -> gs
    Just s -> if atLastZ (gZipper s) then
                onFocused goFirstZ l (focusGroupDown l gs)
              else
                focusDown l gs

swapPrevZ :: ModifySpec
swapPrevZ l gs = let f = getFocusZ gs in
  case f of
    Nothing -> gs
    Just (G _ (Just (Stack _ [] []))) -> onFocused (until atLastZ swapDownZ) l (moveToGroupUp True l gs)
    Just s -> if atFirstZ (gZipper s) then
                moveToNewGroupUp l gs
              else
                swapUp l gs

swapNextZ :: ModifySpec
swapNextZ l gs = let f = getFocusZ gs in
  case f of
    Nothing -> gs
    Just (G _ (Just (Stack _ [] []))) -> (moveToGroupDown True l gs)
    Just s -> if atLastZ (gZipper s) then
                moveToNewGroupDown l gs
              else
                swapDown l gs

swapPrev = sendMessage $ Modify $ swapPrevZ
swapNext = sendMessage $ Modify $ swapNextZ

focusPrev = alt focusPrevZ W.focusUp
focusNext = alt focusNextZ W.focusDown

makeGroup = G.moveToNewGroupDown

onFocused :: (Zipper Window -> Zipper Window) -> ModifySpec
onFocused f _ gs = onFocusedZ (onZipper f) gs

-- TODO
-- function to split a group when there is only one
-- and it is in rows layout

-- functions to set group layouts (tabbed / not tabbed)
-- functions to resize windows? might be unneccessary?
-- fullscreen for group vs layout etc.

groupNextLayout = sendMessage $ ToFocused $ SomeMessage $ NextLayout

