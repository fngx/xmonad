{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (addLayout, layoutKeys, preserveFocusOrder) where

import Local.Windows (recentWindows)
import qualified Local.Theme as Theme

import XMonad hiding ( (|||) )
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

wmii s t = G.group inner outer
  where inner = acc ||| irow ||| tabs ||| icol

        as x = renamed [Replace x]
        il n = (as n) . (spacing 0 2)

        acc  = il "A" $ Accordion
        irow = il "R" $ Row.row Row.V
        icol = il "C" $ Row.row Row.H
        tabs = as "T" $ tabbed s t

        outer = renamed [CutWordsLeft 2] $ spacing 4 2 $ ocol ||| Full ||| orow

        ocol = Row.orderRow Row.H
        orow = Row.orderRow Row.V

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         Patch 0 $
         (wmii shrinkText Theme.decorations)

addLayout c =
  c { layoutHook = layout }

layoutKeys =
  [ ("M-n", ("down", alt (focusZ False) W.focusDown))
  , ("M-p", ("up", alt (focusZ True) W.focusUp))

  , ("M-S-n", ("swap down", alt (swapZ False) W.swapDown))
  , ("M-S-p", ("swap up", alt (swapZ True) W.swapUp))

  , ("M-M1-h", ("swap group left", alt (G.swapGroupUp) W.swapUp))
  , ("M-M1-j", ("swap group right", alt (G.swapGroupDown) W.swapDown))

  , ("M-h",  ("group left", alt (G.focusGroupUp) W.focusUp))
  , ("M-j", ("group right", alt (G.focusGroupDown) W.focusDown))

  , ("M-S-h", ("move left", H.moveToGroupUp False))
  , ("M-S-j", ("move right", H.moveToGroupDown False))

  , ("M--", ("shrink H", sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Shrink :: Row.Msg Int)))
  , ("M-=", ("grow H", sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Grow :: Row.Msg Int)))
  , ("M-S--", ("shrink V", sendMessage $ G.ToFocused $ SomeMessage $ (Row.Shrink :: Row.Msg Window)))
  , ("M-S-=", ("grow V", sendMessage $ G.ToFocused $ SomeMessage $ (Row.Grow :: Row.Msg Window)))
  , ("M-'", ("reset", do sendMessage $ G.ToAll $ SomeMessage $ (Row.Equalize :: Row.Msg Window)
                         sendMessage $ G.ToEnclosing $ SomeMessage $ (Row.Equalize :: Row.Msg Int)))

  , ("M-s", ("col right", H.moveToNewGroupDown >> preserveFocusOrder))
  , ("M-S-s", ("col left", H.moveToNewGroupUp >> preserveFocusOrder))
  , ("M-v", ("split col", H.splitGroup >> preserveFocusOrder))

  , ("M-f", ("full", sendMessage $ Toggle FULL))
  , ("M-S-f", ("gfull", sendMessage $ G.ToEnclosing $ SomeMessage $ NextLayout))

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

  , ("M-k", ("kill", kill >> preserveFocusOrder))

  ]
  where
    jump2 o i = do sendMessage $ G.ToAll $ SomeMessage $ JumpToLayout i
                   sendMessage $ G.ToEnclosing $ SomeMessage $ JumpToLayout o
    jumpi i = sendMessage $ G.ToFocused $ SomeMessage $ JumpToLayout i
    cycleInnerLayout = sendMessage $ G.ToFocused $ SomeMessage $ NextLayout

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

autoColumn :: [Window] -> Int -> G.ModifySpec
autoColumn rws n l gs = let count = length $ fst $ toIndex gs in
  if count < n then
    focusWithOrder rws l $ G.moveToNewGroupDown l gs
  else gs

-- this serves to pass relevant messages to sublayouts
data Patch l a = Patch Int (l a) deriving (Show, Read)

instance (LayoutClass l a) => LayoutClass (Patch l) a where
  description (Patch i x) = description x

  runLayout (Workspace t (Patch i l0) ms) scr = do
    let i' = length $ W.integrate' ms
    (rs, ml1) <- runLayout (Workspace t l0 ms) scr
    let l1 = (fromMaybe l0 ml1)
    if i' > i then
      -- todo what if we want to send multiple messages, and what if they depend on X state
      do (Rectangle _ _ sw _) <- gets $ screenRect . W.screenDetail . W.current . windowset
         rws <- recentWindows
         ml2 <- handleMessage l1 $ SomeMessage $ G.Modify $ autoColumn rws $ floor $ (fromIntegral sw) / 800
         let l2 = fromMaybe l1 ml2
         (rs, ml3) <- runLayout (Workspace t l2 ms) scr
         let l3 = fromMaybe l2 ml3
         return $ (rs, Just $ Patch i' l3)
      else if i' < i then return $ (rs, Just $ Patch i' l1)
           else return $ (rs, fmap (Patch i') ml1)

  handleMessage (Patch i l) m = fmap (fmap (\x -> (Patch i x))) $ handleMessage l m'
    where
      m' -- this is a hack to make the tabs update
        | Just e@(ButtonEvent {}) <- fromMessage m = SomeMessage $ G.ToAll $ SomeMessage e
        | Just e@(PropertyEvent {}) <- fromMessage m = SomeMessage $ G.ToAll $ SomeMessage e
        | Just e@(ExposeEvent {}) <- fromMessage m = SomeMessage $ G.ToAll $ SomeMessage e
        | otherwise = m
