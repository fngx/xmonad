module Local.Workspaces (fixedWorkspaces, workspaceKeys) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Actions.Warp

fixedWorkspaces = ["one", "two"]

workspaceKeys = [ ("M-z", swapNextScreen)
                , ("M-S-z", shiftNextScreen)
                , ("M-y", nextScreen >> warp)
                , ("M-.", nextWS)
                , ("M-,", prevWS)

                -- also window keys

                , ("M-u", focusUrgent)
                ] where warp :: X ()
                        warp = do sid <- gets (W.screen . W.current . windowset)
                                  warpToScreen sid 0.1 0.1
                                  warpToWindow 0.1 0.1
