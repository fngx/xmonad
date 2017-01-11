{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (addLayout, layoutKeys, preserveFocusOrder) where

import Local.Windows (recentWindows)
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
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), SetStruts (SetStruts))
import XMonad.Layout.Renamed
import Data.List (find)

wmii s t = G.group inner outer
  where inner = column ||| tabs
        outer = Row.orderRow Row.H
        column = Row.row Row.V
        tabs = renamed [Replace "T"] $ tabbed s t

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         renamed [CutWordsLeft 1] $
         Max.maximize $
         Patch $
         (wmii shrinkText Theme.decorations)

addLayout c =
  c { layoutHook = layout }

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
  , ("M-s", ("col right", H.moveToNewGroupDown >> preserveFocusOrder))
  , ("M-S-s", ("col left", H.moveToNewGroupUp >> preserveFocusOrder))
  , ("M-v", ("split col", H.splitGroup >> preserveFocusOrder))

  , ("M-l", ("next layout", sendMessage $ G.ToFocused $ SomeMessage $ NextLayout))

  , ("M-f", ("full", sendMessage $ Toggle FULL))

  , ("M-m", ("mag", withFocused (sendMessage . Max.maximizeRestore))) -- magnifier

  , ("M-c M-c", ("rows in cols", do sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.SetAxis Row.H :: Row.Msg Int)
                                    sendMessage $ G.ToAll $ SomeMessage $ (Row.SetAxis Row.V :: Row.Msg Window)))
  , ("M-c M-r", ("cols in rows", do sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.SetAxis Row.V :: Row.Msg Int)
                                    sendMessage $ G.ToAll $ SomeMessage $ (Row.SetAxis Row.H :: Row.Msg Window)))

  , ("M-c c", ("flip cols", sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Flip :: Row.Msg Int)))
  , ("M-c r", ("flip rows", sendMessage $ G.ToFocused $ SomeMessage $ (Row.Flip :: Row.Msg Window)))

  -- just incase I want a dock?
  -- could show me the ws I am on and window title and whether fullscreen?
  -- and the date and battery and VPN

  , ("M-b", ("dock", (broadcastMessage ToggleStruts) >> refresh))
  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >> refresh))
  , ("M-M1-b", ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >> refresh))

  , ("M-k", ("kill", kill >> preserveFocusOrder))
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

focusWithOrder :: [Window] -> G.ModifySpec
focusWithOrder rws l gs = mapZ (G.onZipper . refocus) gs
  where
    refocus True st = st
    refocus False st =
      let (items, focus) = toIndex st
          mostRecent = find (flip elem items) rws in
        maybe st (\f -> until ((==) (Just f) . getFocusZ) focusDownZ $ focusMasterZ st) mostRecent

preserveFocusOrder :: X ()
preserveFocusOrder = do
  rws <- recentWindows
  sendMessage $ (G.Modify $ focusWithOrder rws)
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
