module Local.Windows (recentWindows, windowKeys, greedyFocusWindow) where

import Control.Monad
import Data.List
import Data.Maybe
import Local.Util
import Local.Workspaces (warp)
import XMonad
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows (getName, unName, NamedWindow)
import qualified Local.Theme as Theme
import qualified XMonad.StackSet as W

recentWindows :: X [Window]
recentWindows = (fmap W.allWindows (gets windowset))

greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s

windowKeys = [ ("M-u", focusUrgent ) ]
