module XMonad.Actions.BringFrom where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowBringer (bringWindow)
import Data.Maybe (isJust, fromJust, listToMaybe)

bringFrom w = windows $ bringHeadOfMin w

bringHeadOfMin :: String -> WindowSet -> WindowSet
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
