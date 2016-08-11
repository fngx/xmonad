{-# LANGUAGE Rank2Types, FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Rows where

import XMonad.Core ( SomeMessage(..) , LayoutClass(..) )
import XMonad (sendMessage, Window, ChangeLayout(NextLayout), X, WindowSet, windows, withFocused, Full(..))
--import XMonad.Layout (Full (Full))
import XMonad.StackSet (Stack (Stack))
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import XMonad.Layout.Row (row, orderRow, Axis (H, V), axis, isOuterLayout, Msg(..))
import XMonad.Layout.Tabbed (tabbed, shrinkText)
import XMonad.Layout.LayoutCombinators ( (|||) , JumpToLayout (JumpToLayout, Wrap) )
import XMonad.Layout.Decoration (def, fontName, decoHeight
                                , inactiveBorderColor , activeBorderColor
                                , activeColor, inactiveColor
                                , inactiveTextColor , activeTextColor
                                , urgentTextColor, urgentBorderColor, urgentColor
                                )

import qualified XMonad.Layout.Groups.Helpers as G
import XMonad.Layout.Groups
import Control.Monad (unless, when)
import qualified Data.Set as S
import XMonad.Actions.MessageFeedback (send)
import Data.Maybe ( Maybe(..), fromMaybe, fromJust, isNothing )
import XMonad.Layout.Renamed (renamed, Rename(Replace))

myTheme tc = def
  { fontName = "xft:Monospace-8"
  , decoHeight = 16
  , inactiveBorderColor = "#444444"
  , activeBorderColor   = tc
  , activeColor         = tc
  , inactiveColor       = "#333333"
  , inactiveTextColor   = "#888888"
  , activeTextColor     = "black"
  , urgentBorderColor   = "red"
  , urgentColor         = "red"
  , urgentTextColor     = "white"
  }

rows tc = let t = renamed [Replace "T"] $ tabbed shrinkText $ myTheme tc
              inner = row V ||| t
              outer = (orderRow H ||| Full)
          in balance $ group inner outer

balance x = Balanced 0 x

data Balanced l a = Balanced Int (l a) deriving (Read, Show)

stackSize Nothing = 0
stackSize (Just (W.Stack a u d)) = 1+(length u)+(length d)

instance (LayoutClass l a) => LayoutClass (Balanced l) a where
  description (Balanced _ l) = description l

  runLayout (W.Workspace t (Balanced size st) ms) r =
    let size' = stackSize ms in
      if size' <= size then do
        (rs, mst) <- runLayout (W.Workspace t st ms) r
        return (rs, fmap (\x -> (Balanced size' x)) mst)
      else do
        (rs, mst) <- runLayout (W.Workspace t st ms) r
        mst' <- handleMessage (fromMaybe st mst) (SomeMessage $ Modify rebalance)
        if isNothing mst' then return (rs, Just $ Balanced size' (fromMaybe st mst))
          else do (rs', mst'') <- runLayout (W.Workspace t (fromJust mst') ms) r
                  return (rs', Just $ Balanced size' (fromMaybe (fromJust mst') mst''))

  handleMessage (Balanced n l) m = fmap (fmap (\x -> (Balanced n x))) $ handleMessage l m

rebalance :: ModifySpec
rebalance l gs@(Just (Stack (G gl s) [] []))
  | multipleWindows s -- && isRow gl
  = moveToNewGroupDown l gs
  | otherwise = gs
  where multipleWindows (Just (Stack _ [] [])) = False
        multipleWindows (Just (Stack _ _ _)) = True
        multipleWindows _ = False
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

maximize = withFocused $ \w -> sendMessage $ ToFocused $ SomeMessage $ Maximize w
equalize = sendMessage $ ToFocused $ SomeMessage $ (Equalize :: Msg Window)

-- to max the focused column, I need its index which is I think not
-- accessible readily. option B here is to retain the last focused
-- element so I can target that

maximizeC = sendMessage $ ToEnclosing $ SomeMessage $ (MaximizeLast :: Msg Int)
equalizeC = sendMessage $ ToEnclosing $ SomeMessage $ (Equalize :: Msg Int)
outerNextLayout = sendMessage $ ToEnclosing $ SomeMessage $ NextLayout
