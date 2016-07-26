{-# LANGUAGE Rank2Types #-}
module XMonad.Layout.Rows where

import XMonad.Core ( SomeMessage(..) )
import XMonad (sendMessage, Window, ChangeLayout(NextLayout))
import XMonad.StackSet (Stack (Stack))
import XMonad.Util.Stack
import XMonad.Layout.Row (row, orderRow, Axis (H, V), axis, isOuterLayout)
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.LayoutCombinators ( (|||) , JumpToLayout (JumpToLayout, Wrap) )

-- maybe
import qualified XMonad.Layout.Groups.Helpers as G
import XMonad.Layout.Groups

rows = let inner = row V ||| simpleTabbed
           outer = orderRow H
       in group inner outer

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

focusPrev = sendMessage $ Modify $ focusPrevZ
focusNext = sendMessage $ Modify $ focusNextZ

makeGroup = G.moveToNewGroupDown

onFocused :: (Zipper Window -> Zipper Window) -> ModifySpec
onFocused f _ gs = onFocusedZ (onZipper f) gs

-- TODO
-- function to split a group when there is only one
-- and it is in rows layout

-- functions to set group layouts (tabbed / not tabbed)
-- functions to resize windows? might be unneccessary?
-- fullscreen for group vs layout etc.

groupToTabbed = sendMessage $ JumpToLayout "Tabbed"

groupNextLayout = sendMessage $ ToFocused $ SomeMessage $ NextLayout
