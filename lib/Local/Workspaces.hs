module Local.Workspaces (fixedWorkspaces, workspaceKeys, warp, nonEmptyNames, workspaceNames) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Warp
import XMonad.Util.WorkspaceCompare ( getSortByIndex )
import Data.Maybe (isJust)

fixedWorkspaces = ["one", "two"]

workspaceKeys = [ ("M-d M-d", ("screen swap", swapNextScreen >> warp))
                , ("M-d M-s", ("screen shift", shiftNextScreen >> nextScreen >> warp))
                , ("M-d M-f", ("screen focus", nextScreen >> warp))
                ] ++
                [("M-" ++ show n, ("view " ++ show n, withNthNEWorkspace W.greedyView (n-1))) | n <- [1 .. 9]] ++
                [("M-S-" ++ show n, ("view " ++ show n, withNthNEWorkspace W.shift (n-1))) | n <- [1 .. 9]]

warp :: X ()
warp = do mf <- gets (W.peek . windowset)
          case mf of
            (Just _) -> warpToWindow 0.1 0.1
            _ -> do sid <- gets (W.screen . W.current . windowset)
                    warpToScreen sid 0.1 0.1

withNthNEWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthNEWorkspace job wnum = do ws <- nonEmptyNames
                                 case drop wnum ws of
                                   (w:_) -> windows $ job w
                                   [] -> return ()

--not quite right
nonEmptyNames :: X [WorkspaceId]
nonEmptyNames = do sort <- getSortByIndex
                   ws <- gets windowset
                   let spaces = (map W.workspace ((W.current ws):(W.visible ws))) ++
                                (filter (isJust . W.stack) $ W.hidden ws)
                   return $ map W.tag $ sort spaces

workspaceNames :: X [WorkspaceId]
workspaceNames = do sort <- getSortByIndex
                    gets (map W.tag . sort . W.workspaces . windowset)
