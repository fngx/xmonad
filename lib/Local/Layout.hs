{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (layout, layoutKeys) where

import qualified Local.Theme as Theme

import XMonad
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as H
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
import XMonad.Layout
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Actions.MessageFeedback
import Control.Monad (unless, when)
import qualified Local.Row as Row
import qualified XMonad.Layout.Maximize as Max

wmii s t = G.group inner outer
  where inner = column ||| tabs
        outer = Row.orderRow Row.H
        column = Row.row Row.V
        tabs = tabbed s t

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         Max.maximize $
         Patch $
         (wmii shrinkText Theme.decorations)

layoutKeys =
  [ ("M-n", ("down", alt (focusZ False) W.focusDown))
  , ("M-p", ("up", alt (focusZ True) W.focusUp))

  , ("M-S-n", ("swap down", alt (swapZ False) W.swapDown))
  , ("M-S-p", ("swap up", alt (swapZ True) W.swapUp))

  , ("M-M1-n", ("swap group right", alt (G.swapGroupDown) W.swapDown))
  , ("M-M1-p", ("swap group left", alt (G.swapGroupUp) W.swapUp))

  , ("M-<Tab>", ("group left", alt (G.focusGroupDown) W.focusDown))
  , ("M-S-<Tab>", ("group right", alt (G.focusGroupUp) W.focusUp))

  , ("M-C-p", ("move left", H.moveToGroupUp False))
  , ("M-C-n", ("move right", H.moveToGroupDown False))

  , ("M--", ("shrink H", sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Shrink :: Row.Msg Int)))
  , ("M-=", ("grow H", sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Grow :: Row.Msg Int)))
  , ("M-S--", ("shrink V", sendMessage $ G.ToFocused $ SomeMessage $ (Row.Shrink :: Row.Msg Window)))
  , ("M-S-=", ("grow V", sendMessage $ G.ToFocused $ SomeMessage $ (Row.Grow :: Row.Msg Window)))
  , ("M-'", ("reset", do sendMessage $ G.ToAll $ SomeMessage $ (Row.Equalize :: Row.Msg Window)
                         sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Equalize :: Row.Msg Int)))
  , ("M-s", ("col right", H.moveToNewGroupDown))
  , ("M-S-s", ("col left", H.moveToNewGroupUp))
  , ("M-v", ("split col", H.splitGroup))

  , ("M-l", ("next layout", sendMessage $ G.ToFocused $ SomeMessage $ NextLayout))

  , ("M-f", ("full", sendMessage $ Toggle FULL))

  , ("M-m", ("mag", withFocused (sendMessage . Max.maximizeRestore))) -- magnifier

  , ("M-i M-o", ("flip outer", sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Flip :: Row.Msg Int)))
  , ("M-i M-i", ("flip inner", sendMessage $ G.ToFocused $ SomeMessage $ (Row.Flip :: Row.Msg Window)))
  , ("M-i r", ("rows in cols", do sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.SetAxis Row.H :: Row.Msg Int)
                                  sendMessage $ G.ToAll $ SomeMessage $ (Row.SetAxis Row.V :: Row.Msg Window)))
  , ("M-i c", ("cols in rows", do sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.SetAxis Row.V :: Row.Msg Int)
                                  sendMessage $ G.ToAll $ SomeMessage $ (Row.SetAxis Row.H :: Row.Msg Window)))

  ]

-- movement operators

alt :: G.ModifySpec -> (WindowSet -> WindowSet) -> X ()
alt f g = alt2 (G.Modify f) $ windows g

alt2 :: G.GroupsMessage -> X () -> X ()
alt2 m x = do b <- sendSM $ SomeMessage m
              unless b x
              when b $ refresh

atEnd _ Nothing = True
atEnd b (Just (Stack f u d)) = null $ if b then u else d

goEnd b = until (atEnd b) $ if b then focusDownZ else focusUpZ

focusZ :: Bool -> G.ModifySpec
focusZ b l gs = let f = getFocusZ gs in
  case f of Nothing -> gs
            Just s -> if (atEnd b) (G.gZipper s) then
                        onFocused (goEnd $ not b) l (moveGroup l gs)
                      else
                        moveInGroup l gs
  where
    moveGroup = if b then G.focusGroupUp else G.focusGroupDown
    moveInGroup = if b then G.focusUp else G.focusDown


swapZ :: Bool -> G.ModifySpec
swapZ b l gs = let f = getFocusZ gs in
  case f of
    Nothing -> gs
    Just (G.G _ (Just (Stack _ [] []))) ->
      onFocused (until (atEnd $ not b) swap') l (moveToGroup True l gs)
    Just s -> if (atEnd b) (G.gZipper s) then
                moveToNewGroup l gs
              else
                swap l gs
  where
    moveToGroup = if b then G.moveToGroupUp else G.moveToGroupDown
    moveToNewGroup = if b then G.moveToNewGroupUp else G.moveToNewGroupDown
    swap = if b then G.swapUp else G.swapDown
    swap' = if b then swapDownZ else swapUpZ

onFocused f _ gs = onFocusedZ (G.onZipper f) gs

-- begin hack for tabs

data Patch l a = Patch (l a) deriving (Show, Read)

instance (LayoutClass l a) => LayoutClass (Patch l) a where
  description (Patch x) = description x

  runLayout (Workspace t (Patch ml) ms) scr = do
    (rs, ml) <- runLayout (Workspace t ml ms) scr
    return $ (rs, fmap Patch ml)

  handleMessage (Patch l) m = fmap (fmap (\x -> (Patch x))) $ handleMessage l m'
    where
      m' -- this is a hack to make the tabs update
        | Just e@(ButtonEvent {}) <- fromMessage m = SomeMessage $ G.ToAll $ SomeMessage e
        | Just e@(PropertyEvent {}) <- fromMessage m = SomeMessage $ G.ToAll $ SomeMessage e
        | Just e@(ExposeEvent {}) <- fromMessage m = SomeMessage $ G.ToAll $ SomeMessage e
        | otherwise = m
