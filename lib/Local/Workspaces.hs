module Local.Workspaces (fixedWorkspaces, workspaceKeys, warp, nonEmptyNames, workspaceNames) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Warp
import XMonad.Util.WorkspaceCompare ( getSortByIndex )
import Data.Maybe (isJust, isNothing)
import Data.List ( (\\) )

fixedWorkspaces = ["1", "2"]

workspaceKeys =
  let swapS = ("screen swap", swapNextScreen >> warp)
      focusS = ("screen focus", nextScreen >> warp)
      shiftS = ("screen shift", shiftNextScreen >> nextScreen >> warp)
      view n = ("view " ++ show n, withNthNEWorkspace W.greedyView (n-1))
      shiftTo n = ("shift to " ++ show n, withNthNEWorkspace W.shift (n-1))
      onEmpty a = ("view empty", do en <- emptyNames
                                    an <- workspaceNames
                                    a $ head $ en ++ ([show n | n <- [1..]] \\ an))
  in [ ("M-d M-d", swapS)
     , ("M-d M-s", shiftS)
     , ("M-d M-f", focusS)
--     , ("M-z", focusS)
     , ("M-e", onEmpty addWorkspace)
     , ("M-S-e", onEmpty (\w -> addHiddenWorkspace w >> windows (W.shift w) >> windows (W.view w)))
     ] ++
     [("M-" ++ show n, view n) | n <- [1 .. 9]] ++
     [("M-S-" ++ show n, shiftTo n) | n <- [1 .. 9]]

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


emptyNames :: X [WorkspaceId]
emptyNames = do sort <- getSortByIndex
                gets (map W.tag . sort . (filter (isNothing . W.stack)) . W.workspaces . windowset)

nonEmptyNames :: X [WorkspaceId]
nonEmptyNames = do sort <- getSortByIndex
                   ws <- gets windowset
                   let spaces = (map W.workspace ((W.current ws):(W.visible ws))) ++
                                (filter (isJust . W.stack) $ W.hidden ws)
                   return $ map W.tag $ sort spaces

workspaceNames :: X [WorkspaceId]
workspaceNames = do sort <- getSortByIndex
                    gets (map W.tag . sort . W.workspaces . windowset)
