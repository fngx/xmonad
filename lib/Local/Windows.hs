{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, recentWindows, windowKeys, greedyFocusWindow, nextInHistory) where

import XMonad.Actions.TagWindows

import Control.Applicative ((<$>))
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import Local.Util
import Local.Workspaces (warp)
import XMonad
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows (getName, unName, NamedWindow)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import XMonad.Util.NamedWindows (getName, unName)
import Data.Monoid
import Local.Hints (repeatHintedKeys)
import XMonad.Actions.CycleWindows (rotUp, rotDown)
import qualified Local.MC as MC

import XMonad.Actions.WindowBringer (bringWindow)

import Data.IORef
import XMonad.Util.XUtils
import XMonad.Util.Font

data WindowHistory = WH (Maybe Window) (Seq Window)
  deriving (Typeable, Read, Show)

instance ExtensionClass WindowHistory where
  initialValue = WH Nothing empty
  extensionType = PersistentExtension

updateHistory :: WindowHistory -> X WindowHistory
updateHistory input = withWindowSet $ \ss -> do
  insertHistory (W.peek ss) input

insertHistory :: Maybe Window -> WindowHistory -> X WindowHistory
insertHistory newcur (WH oldcur oldhist) = withWindowSet $ \ss -> do
  let wins     = Set.fromList $ W.allWindows ss
      newhist  = Seq.filter (flip Set.member wins) (ins oldcur oldhist)
  return $ WH newcur (del newcur newhist)
  where
    ins x xs = maybe xs (<| xs) x
    del x xs = maybe xs (\x' -> Seq.filter (/= x') xs) x

recentWindows :: X [Window]
recentWindows = do
  hook
  (WH cur hist) <- XS.get
  return $ (maybeToList cur) ++ (toList hist)

greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s

navKeys = [ ("M-o",   ("next", focusNext >> warp))
          , ("M-i",   ("prev", focusPrev >> warp))
          , ("M-n",   ("down", windows W.focusDown >> warp))
          , ("M-p",   ("up",   windows W.focusUp >> warp))
          , ("M-S-N", ("down", windows W.swapDown >> warp))
          , ("M-S-P", ("up",   windows W.swapUp >> warp))
          , ("M-m",   ("master", windows W.focusMaster >> warp))
          , ("M-S-M", ("swap m", (windows W.swapMaster) >> warp))
          , ("M-S-o", ("swap o", (bringFocusNext >> (windows W.swapMaster)) >> warp))
          ]

swapWith = do wset <- gets windowset
              let visWindows :: [Window]
                  visWindows =
                    (maybe [] W.down $ W.stack $ W.workspace $ W.current $ wset) ++
                    (concatMap (W.integrate' . W.stack . W.workspace) (W.visible wset)) ++
                    (maybe [] (Data.List.reverse . W.up) $ W.stack $ W.workspace $ W.current $ wset)
              wref <- io $ newIORef visWindows
              let rot step = do ws <- io $ readIORef wref
                                io $ modifyIORef wref step
                                ws' <- io $ readIORef wref
                                delTag "S" (head ws)
                                addTag "S" (head ws')
              rot id
              repeatHintedKeys [ ("M-.", ("next", rot rotUp)) ,
                                 ("M-,", ("prev", rot rotDown)) ]

              withTaggedGlobal "S" $ delTag "S"
              ws <- io $ readIORef wref
              -- I want to swap the window for the focused instead
              windows $ let choice = head ws
                            replace old new l = flip map l $ \x -> if x == old then new else x
                        in
                          W.modify' $ \(W.Stack f u d) -> W.Stack choice
                                                          (replace choice f u)
                                                          (replace choice f d)

navigate action = do history <- XS.get :: X WindowHistory
                     action
                     warp
                     nav
                     updateHistory history >>= XS.put
                     withTaggedGlobal "." $ delTag "."
                       where nav = repeatHintedKeys navKeys


windowKeys = [ ("M-o", ("next", navigate (focusUrgentOr focusNext)))
             , ("M-n", ("down", navigate $ windows W.focusDown))
             , ("M-p", ("up", navigate $ windows W.focusUp))
             , ("M-S-o", ("swap o", navigate $ (bringFocusNext >> (windows W.swapMaster)) >> warp))
             , ("M-t", ("floaty",
                        withFocused $ \w -> do
                           isFloating <- gets (M.member w . W.floating . windowset)
                           if isFloating then windows $ W.sink w
                             else floatTo (0.6, 0.95) (0.05, 0.4) w
                         ))
             , ("M-.", ("swap selection",
                        swapWith
                       ))
             , ("M-,", ("swap selection",
                        swapWith
                       ))

             ]

focusNth n = windows $ foldr (.) W.focusMaster (Data.List.take n $ repeat W.focusDown)

focusUrgentOr a = do us <- readUrgents
                     if Data.List.null us then a else (focusUrgent >> warp)

bringFocusNext = do withFocused $ addTag "."
                    rw <- recentWindows >>= filterM ((fmap not) . hasTag ".")
                    whenJust (listToMaybe rw) $ \w -> do
                      windows $ \ss -> (W.focusWindow w (bringWindow w ss))

focusNext = do withFocused $ addTag "."
               rw <- recentWindows >>= filterM ((fmap not) . hasTag ".")
               whenJust (listToMaybe rw) $ windows . W.focusWindow

focusPrev = do withFocused $ delTag "."
               rw <- recentWindows >>= filterM (hasTag ".")
               whenJust (listToMaybe rw) $ windows . W.focusWindow

nextInHistory = recentWindows >>=
                  (return . (Data.List.drop 1)) >>=
                  (filterM ((fmap not) . hasTag ".")) >>=
                  (return . listToMaybe)

addHistory c = c { logHook = hook >> (logHook c)
                 }

hook :: X ()
hook = do focusM <- gets (W.peek . windowset)
          (WH lf _) <- XS.get
          when (lf /= focusM) $
            do XS.get >>= updateHistory >>= XS.put

getWindowRect :: Window -> X Rectangle
getWindowRect w = do d <- asks display
                     atts <- io $ getWindowAttributes d w
                     let bw = wa_border_width atts
                     return $ Rectangle
                       (fromIntegral $ wa_x atts)
                       (fromIntegral $ wa_y atts)
                       (fromIntegral $ bw + wa_width atts)
                       (fromIntegral $ bw + wa_height atts)

floatTo :: (Rational, Rational) -> (Rational, Rational) -> Window -> X ()
floatTo (left, right) (top, bottom) w = withDisplay $ \d -> do
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset
  let nw = fi $ round $ (fi sw) * (right - left)
      nh = fi $ round $ (fi sh) * (bottom - top)
      nx = fi $ round $ (fi sx + fi sw) * left
      ny = fi $ round $ (fi sy + fi sh) * top
  io $ raiseWindow d w
  io $ resizeWindow d w nw nh
  io $ moveWindow d w nx ny
  float w
