{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (layout, layoutKeys) where

import qualified Local.Theme as Theme

import XMonad hiding ((|||))
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as H
import XMonad.StackSet (Workspace (Workspace), Stack (..))
import XMonad.Util.Stack
import XMonad.Layout.TrackFloating
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Groups.Wmii
import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize

layout = showWName $
         trackFloating $
         smartBorders $
         magnifierOff $
         maximize $
         ((Patch $ wmii shrinkText Theme.decorations) ||| Full)

layoutKeys =
  [ ("M-j", sendMessage $ G.Modify $ focusZ False)
  , ("M-k", sendMessage $ G.Modify $ focusZ True)

  , ("M-S-j", sendMessage $ G.Modify $ swapZ False)
  , ("M-S-k", sendMessage $ G.Modify $ swapZ True)

  , ("M-o", H.moveToNewGroupDown)
  , ("M-S-o", H.splitGroup)

  , ("M-<Space>", sendMessage NextLayout)
  , ("M-;", groupToNextLayout)

  , ("M-S-m", sendMessage Toggle)
  , ("M-m", withFocused (sendMessage . maximizeRestore))
  ]

-- movement operators

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
