module Local.Workspaces (fixedWorkspaces, workspaceKeys, warp, nonEmptyNames, workspaceNames, autoWorkspaceNames) where

import Control.Monad (when)

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.Warp
import XMonad.Util.WorkspaceCompare ( getSortByIndex )
import Data.Maybe (isJust, isNothing, listToMaybe, fromMaybe)
import Data.List ( (\\) )
import Control.Applicative ( (<$>) )
import Control.Monad ( join )
import Local.Hints (repeatHintedKeys)

fixedWorkspaces = []
minT = "zap"
autoWorkspaceNames = do en <- emptyNames
                        an <- workspaceNames
                        return $ (en \\ [minT]) ++ ([x:[] | x <- ['A' .. 'Z']] \\ an)
workspaceKeys =
  let swapS = ("screen swap", swapNextScreen >> warp)
      focusS = ("screen focus", nextScreen >> warp)
      shiftS = ("screen shift", shiftNextScreen >> nextScreen >> warp)
      doView n = withNthNEWorkspace W.greedyView (n-1)
      view n = ("view " ++ show n, doView n)
      shiftTo n = ("shift to " ++ show n, withNthNEWorkspace W.shift (n-1))
      onEmpty a = ("view empty", do auto <- autoWorkspaceNames
                                    a $ head auto)

      masterOf :: String -> WindowSet -> Maybe Window
      masterOf tag ss = join $
                        ((listToMaybe . W.integrate' . W.stack) <$>
                         (listToMaybe $ filter ((== tag) . W.tag) $ W.workspaces ss))

      sendT = do addHiddenWorkspace minT
                 windows (W.shift minT)
      bringT = windows $ \ss -> fromMaybe ss $ (flip bringWindow ss) <$> (masterOf minT ss)

      after x [] = x
      after x (a:(y:xs))
        | x == a = y
        | otherwise = after x (y:xs)

      onOtherScreen a = nextScreen >> a >> prevScreen
  in [ ("M-s", swapS)
     , ("M-/", swapS)
     , ("M-S-s", shiftS)
     , ("M-d", focusS)
     , ("M-;", focusS)
     , ("M-C-n",   ("view next",  moveTo Next HiddenNonEmptyWS))
     , ("M-C-p", ("view prev", moveTo Prev HiddenNonEmptyWS))
     , ("M-e",   onEmpty addWorkspace)
     , ("M-S-e", onEmpty (\w -> addHiddenWorkspace w >> windows (W.shift w) >> windows (W.view w)))
     , ("M-h",   ("send to " ++ minT, sendT))
     , ("M-S-h", ("bring from " ++ minT, do bringT
                                            -- could do gets to find all minimized windows
                                            -- and then cycle through them
                                            minWs' <- gets
                                              (concat .
                                               (map (W.integrate' . W.stack)) .
                                               (filter ((== minT) . W.tag)) .
                                               W.workspaces .
                                               windowset)
                                            withFocused $ \fw ->
                                              let minWs = fw:minWs' in
                                                repeatHintedKeys [("M-S-H", ("another",
                                                                             withFocused $ \fw -> do
                                                                                sendT
                                                                                windows $ bringWindow $ after fw (cycle minWs)
                                                                                ))]
                 ))
     ] ++
     (concat $
      [[ ("M-" ++ show n, view n)
       , ("M-S-" ++ show n, shiftTo n)
       , ("M-M1-" ++ (show n), ("view other " ++ (show n), onOtherScreen $ doView n)) ]
      | n <- [1 .. 9]]) ++
     (concat $ [[ ("M-" ++ c, view n), ("M-S-" ++ c, shiftTo n) ] | (c, n) <- zip ["z", "x", "c", "v"] [1 .. 4]])


warp :: X ()
warp = do mf <- gets (W.peek . windowset)
          case mf of
            (Just _) -> warpToWindow 0.1 0.1
            _ -> do sid <- gets (W.screen . W.current . windowset)
                    warpToScreen sid 0.1 0.1

withNthNEWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthNEWorkspace job wnum = do ws <- nonEmptyNames
                                 thisWs <- gets (W.tag . W.workspace . W.current . windowset)
                                 alt <- nextNonEmptyVis
                                 case drop wnum ws of
                                   (w:_) -> windows $ job $ if w == thisWs
                                                            then alt
                                                            else w
                                   [] -> return ()

emptyNames :: X [WorkspaceId]
emptyNames = do sort <- getSortByIndex
                gets (map W.tag . sort . (filter (isNothing . W.stack)) . W.workspaces . windowset)

nextNonEmptyVis :: X WorkspaceId
nextNonEmptyVis = gets (W.tag . head . tail . (filter (isJust . W.stack)) . W.workspaces . windowset)

nonEmptyNames :: X [WorkspaceId]
nonEmptyNames = do sort <- getSortByIndex
                   ws <- gets windowset
                   let spaces = (map W.workspace ((W.current ws):(W.visible ws))) ++
                                (filter (isJust . W.stack) $ W.hidden ws)
                   return $ map W.tag $ sort spaces

workspaceNames :: X [WorkspaceId]
workspaceNames = do sort <- getSortByIndex
                    gets (map W.tag . sort . W.workspaces . windowset)
