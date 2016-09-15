module XMonad.Actions.Iconify where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowBringer (bringWindow)
import Data.Maybe (fromMaybe, isJust, listToMaybe, fromJust)

-- Ideally I will write a focusWindow and greedyFocusWindow which bring from * when needed
focusWindow' :: (WorkspaceId -> WindowSet -> WindowSet) -> WorkspaceId -> Window -> WindowSet -> WindowSet
focusWindow' v minWs w s | Just w == W.peek s = s
                         | otherwise        = fromMaybe s $ do
                             n <- W.findTag w s
                             let go = until ((Just w ==) . W.peek) W.focusUp (v n s)
                                 bring = bringWindow w s
                             return $ if n == minWs then bring else go

greedyFocusWindow = focusWindow' W.greedyView
focusWindow = focusWindow' W.view

iconify w = windows $ W.shift w

uniconify w = windows $ bringFirst w

bringFirst :: String -> WindowSet -> WindowSet
bringFirst w ws =
          let minSpaces = filter ((== w) . W.tag) $ W.workspaces ws
              firstMin :: Maybe Window
              firstMin =
                do thews <- listToMaybe minSpaces
                   let wins = W.integrate' $ W.stack $ thews
                   win <- listToMaybe wins
                   return $ win
          in if isJust firstMin then bringWindow (fromJust firstMin) ws
             else ws

