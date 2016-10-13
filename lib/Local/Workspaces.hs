module Local.Workspaces (fixedWorkspaces, workspaceKeys) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.UrgencyHook (focusUrgent)

fixedWorkspaces = ["one", "two"]

workspaceKeys = [ ("M-a", toggleWS)
                , ("M-z", swapNextScreen)
                , ("M-S-z", shiftNextScreen)
                , ("M-x", nextScreen)
                , ("M-h", nextWS)
                , ("M-l", prevWS)

                -- also window keys

                , ("M-u", focusUrgent)
                ]
