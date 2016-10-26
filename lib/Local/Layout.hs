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
import qualified XMonad.Layout.Magnifier as Mag
import Control.Monad (unless, when)

wmii s t = G.group inner outer
  where inner = column ||| tabs
        outer = zoomRowG ||| Mirror zoomRowG
        column = Mirror zoomRow
        tabs = tabbed s t

layout = showWName' SWNC
         { swn_font = "xft:Liberation Sans-24"
         , swn_fade = 2
         , swn_bgcolor = Theme.secondaryColor
         , swn_color = Theme.secondaryText
         }
         $
         trackFloating $
         smartBorders $
         mkToggle (single FULL) $
         Mag.magnifierOff $
         (Patch $ wmii shrinkText Theme.decorations)

layoutKeys =
  [ ("M-j", alt (focusZ False) W.focusDown)
  , ("M-k", alt (focusZ True) W.focusUp)

  , ("M-S-j", alt (swapZ False) W.swapDown)
  , ("M-S-k", alt (swapZ True) W.swapUp)

  , ("M--", sendMessage $ G.ToEnclosing $ SomeMessage $ Zoom (3/5))
  , ("M-=", sendMessage $ G.ToEnclosing $ SomeMessage $ Zoom (1/(3/5)))
  , ("M-S--", sendMessage $ G.ToFocused $ SomeMessage $ Zoom (3/5))
  , ("M-S-=", sendMessage $ G.ToFocused $ SomeMessage $ Zoom (1/(3/5)))
  , ("M-'", sendMessage $ G.ToFocused $ SomeMessage $ zoomReset)

  , ("M-o", H.moveToNewGroupDown)
  , ("M-S-o", H.moveToNewGroupUp)

  , ("M-<Space>", sendMessage $ G.ToEnclosing $ SomeMessage $ NextLayout)
  , ("M-;", sendMessage $ G.ToFocused $ SomeMessage $ NextLayout)

  , ("M-f", sendMessage $ Toggle FULL)
  , ("M-m", sendMessage Mag.Toggle) -- magnifier
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
