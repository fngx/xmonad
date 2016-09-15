module XMonad.Actions.MinActions where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowBringer (bringWindow)
import Data.Maybe (isJust, fromJust, listToMaybe, fromMaybe)

bringFrom w = windows $ bringHeadOfMin w
 where bringHeadOfMin :: String -> WindowSet -> WindowSet
       bringHeadOfMin w ws =
          let minSpaces = filter ((== w) . W.tag) $ W.workspaces ws
              firstMin :: Maybe Window
              firstMin =
                do thews <- listToMaybe minSpaces
                   let wins = W.integrate' $ W.stack $ thews
                   win <- listToMaybe wins
                   return $ win
          in if isJust firstMin then bringWindow (fromJust firstMin) ws
             else ws


focusWindow' :: WorkspaceId -> (WorkspaceId -> WindowSet -> WindowSet) -> Window -> WindowSet -> WindowSet
focusWindow' minWs v w s | Just w == W.peek s = s
                   | otherwise        = fromMaybe s $ do
                       n <- W.findTag w s
                       let go = until ((Just w ==) . W.peek) W.focusUp (v n s)
                           bring = bringWindow w s
                       return $ if n == minWs then bring else go

greedyFocusWindow m = focusWindow' m W.greedyView
focusWindow m = focusWindow' m W.view
