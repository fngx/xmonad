module Local.Workspaces (fixedWorkspaces, workspaceKeys) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Hooks.UrgencyHook (readUrgents, clearUrgents)
import XMonad.Actions.Warp
import Local.Prompts (rotWindow, RingDirection (..))

fixedWorkspaces = ["one", "two"]

workspaceKeys = [ ("M-h", swapNextScreen)
                , ("M-S-h", shiftNextScreen)
                , ("M-y", nextScreen >> warp)
                , ("M-l", nextWS)
                , ("M-S-l", prevWS)
                , ("M-.", warp)

                -- also window keys

                , ("M-u", do us <- readUrgents
                             if null us
                               then rotWindow Local.Prompts.Next
                               else windows $ W.focusWindow $ head us)

                , ("M-S-u", do us <- readUrgents
                               if null us
                                 then rotWindow Local.Prompts.Prev
                                 else clearUrgents)

                ] where warp :: X ()
                        warp = do sid <- gets (W.screen . W.current . windowset)
                                  warpToScreen sid 0.1 0.1
                                  warpToWindow 0.1 0.1
