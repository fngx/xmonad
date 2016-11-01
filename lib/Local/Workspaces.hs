module Local.Workspaces (fixedWorkspaces, workspaceKeys, warp) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Warp

fixedWorkspaces = ["one", "two"]

workspaceKeys = [ ("M-h", swapNextScreen)
                , ("M-S-h", shiftNextScreen)
                , ("M-y", nextScreen >> warp)
                , ("M-l", nextWS)
                , ("M-S-l", prevWS)
                , ("M-.", warp)
                ]
warp :: X ()
warp = do sid <- gets (W.screen . W.current . windowset)
          warpToScreen sid 0.1 0.1
          warpToWindow 0.1 0.1
