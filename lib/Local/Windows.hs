module Local.Windows (updateHistory, recentWindows, windowKeys, greedyFocusWindow) where

import XMonad
import Local.Workspaces (warp)
import qualified XMonad.StackSet as W
import qualified Local.Ring as Ring
import XMonad.Util.NamedWindows (getName, unName, NamedWindow)
import Data.Maybe
import Control.Monad
import Data.List
import XMonad.Hooks.UrgencyHook
import Local.Util

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
                          w <- Ring.select windowName (const $ return ())--(windows . W.focusWindow)
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
