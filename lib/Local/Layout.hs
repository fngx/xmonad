{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (addLayout, layoutKeys) where

import Local.Windows (recentWindows)
import Local.Workspaces (warp)
import qualified Local.Theme as Theme

import Data.Maybe (fromJust)

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Layout.Tabbed
import XMonad.StackSet (Workspace (Workspace), Stack (..))
import XMonad.Layout.TrackFloating
import XMonad.Layout.NoBorders
import XMonad.Layout hiding ( (|||) )
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), SetStruts (SetStruts))
import XMonad.Layout.LayoutCombinators
import XMonad.Actions.CycleSelectedLayouts
import Local.MC
import Local.PerScreen
import Local.FullscreenToggleStruts
import XMonad.Layout.Renamed

fat1 = "⓵"
fat2 = "⓶"
fat4 = "⓸"
fat5 = "⓹"

layout = trackFloating $
         smartBorders $
         fullscreenToggleStruts $
         ifWider 1400 choices' choices
  where
    choices  = one  ||| two ||| many ||| lots
    choices' = many ||| two ||| lots ||| one
    two =  aka fat2 $ mct [(1, [1]),   (1, [1])]
    many = aka fat4 $ mct [(1, [2,1]), (1, [2,1])]
    lots = aka fat5 $ mct [(1, [1]), (1, [1,1,1,3])]
    one =  aka fat1 $ mct [(1, [1])]
    mct = mc (tabbed shrinkText Theme.decorations)

    aka n l = renamed [Replace n] l

addLayout c =
  c { layoutHook = layout
    , handleEventHook = mappingEventHook <+> (handleEventHook c)
    }

layoutKeys =
  let col n = (1, take n $ repeat 1)
      sendMC :: MCMsg Window -> X ()
      sendMC = sendMessage

      equalize _ a = zip (repeat 1) $ map ((map (const 1)) . snd) a

      focusMaster = windows W.focusMaster
      focusSecond = sendMC $ FocusCell 1
      focusOverflow = sendMC $ WithOverflowFocus (windows . W.focusWindow)

      ifMaster :: X () -> X () -> X ()
      ifMaster a b = do stm <- gets (W.stack . W.workspace . W.current . windowset)
                        whenJust stm $ \(W.Stack {W.up = up}) -> if (null up) then a else b

  in
  [ ("M-m",    ("focus master", (ifMaster focusSecond focusMaster) >> warp))
  , ("M-S-m",  ("shift master", (ifMaster (focusSecond >> (windows W.swapMaster)) (windows W.shiftMaster)) >> warp))

  , ("M-j",   ("focus 2", focusOverflow >> warp))
  , ("M-S-j", ("focus2 master", sendMC $ WithOverflowFocusIndex $ const 0))

  , ("M-l" ,  ("next", cycleThroughLayouts [fat4, fat2, fat5]))
  , ("M-f",   ("1col", cycleThroughLayouts [fat1, fat2])) -- TODO when we go to a funny layout it breaks
  , ("M-C-<Space>",   ("equalize", withFocused $ (sendMC . ChangeCells equalize)))

  , ("M-=", ("grow", withFocused $ (sendMC . (ResizeCell 0.2 0.2))))
  , ("M--", ("shrink", withFocused $ (sendMC . (ResizeCell (-0.2) (-0.2)))))

  , ("M-M1-n", ("overflow down", sendMC $ WithOverflowFocusIndex (+ 1)))
  , ("M-M1-p", ("overflow up", sendMC $ WithOverflowFocusIndex (flip (-) 1)))

  , ("M-S-n", ("swap down", windows W.swapDown >> warp))
  , ("M-S-p", ("swap up", windows W.swapUp >> warp))

  , ("M-S-f", ("full", withFocused $ \w -> spawn $ "wmctrl -i -r " ++ (show w) ++ " -b toggle,fullscreen"))
  , ("M-S-l", ("flip", sendMC Flip))

  -- sending SIGSTOP to xmobar hangs the output pipe
  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >> refresh))
  , ("M-b",   ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >> refresh))

  , ("M-k", ("kill", kill)) ] ++
  [ ("M-<F" ++ (show n) ++ ">", ("focus nth", (focusNth n) >> warp))
  | n <- [1..6] ]
  where focusNth n = windows (foldl (.) id $ reverse (W.focusMaster:(take (n - 1) $ repeat W.focusDown)))
