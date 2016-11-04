module Local.Windows (updateHistory, recentWindows, windowKeys, greedyFocusWindow) where

import Control.Monad
import Data.List
import Data.Maybe
import Local.Util
import Local.Workspaces (warp)
import XMonad
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows (getName, unName, NamedWindow)
import qualified Local.Ring as Ring
import qualified Local.Theme as Theme
import qualified XMonad.StackSet as W

recentWindows :: X [Window]
recentWindows = do (f, rc) <- Ring.contents
                   ws <- (fmap W.allWindows (gets windowset))

                   return $ rc ++ (ws \\ rc)


greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s


updateHistory = (Ring.update $ fmap (liftM2 (,) W.peek W.allWindows) (gets windowset))

windowKeys = [ ("M-u", do -- shuffle urgent windows to the top?
                          ws <- fmap W.allWindows $ gets windowset
                          readUrgents >>= mapM_ (\w -> Ring.update (return $ (Just w, ws)))
                          updateHistory -- put things back how they are

                          (x0, y0) <- asks theRoot >>= getPointer
                          ss <- gets windowset

                          let onscreen = (W.integrate' (W.stack $ W.workspace $ W.current ss)) ++
                                         (concatMap W.integrate' (map (W.stack . W.workspace) $ W.visible ss))

                          let indicator Nothing = return (x0, y0, Theme.secondaryText, Theme.secondaryColor)
                              indicator (Just w)
                                | w `elem` onscreen =
                                    do (x, y) <- getWindowTopLeft w
                                       return (x, y, Theme.secondaryText, Theme.secondaryColor)
                                | otherwise =
                                    return $ (x0, y0, Theme.focusedText, Theme.focusedBorderColor)

                          w <- Ring.select windowName indicator
                          -- this is a bit silly as it is already focused
                          -- this should preserve the focused screen instead?
                          -- I think what I want is to go through the windows
                          -- but to restore the layout afterwards, which seems hard
                          -- maybe just having a list would do it.
                          whenJust w $ \w -> (windows $ W.focusWindow w) >> warp
               ) ]
             where windowName w = do nam <- getName w
                                     c <- wclass w
                                     t <- fmap (W.findTag w) $ gets windowset
                                     let n = show nam
                                     return $ (fromMaybe "???" t) ++ ": " ++ n ++ " (" ++ c ++ ")"

                -- -- also window keys

                -- , ("M-u", do us <- readUrgents
                --              if null us
                --                then rotWindow Local.Prompts.Next
                --                else windows $ W.focusWindow $ head us)

                -- , ("M-S-u", do us <- readUrgents
                --                if null us
                --                  then rotWindow Local.Prompts.Prev
                --                  else clearUrgents)
