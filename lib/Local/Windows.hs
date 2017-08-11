{-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, windowKeys, greedyFocusWindow, nextInHistory) where

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

data LastWindow = LastWindow {
  currentWindow :: Maybe Window ,
  lastWindow :: Maybe Window
  } deriving (Typeable, Read, Show)

instance ExtensionClass LastWindow where
  initialValue = LastWindow Nothing Nothing
  extensionType = PersistentExtension

updateLastWindow :: Maybe Window -> LastWindow -> LastWindow
updateLastWindow Nothing lw = lw { currentWindow = Nothing }
updateLastWindow nw lw@(LastWindow Nothing _) = lw { currentWindow = nw }
updateLastWindow nw lw@(LastWindow cw _) = lw {currentWindow = nw, lastWindow = cw}

nextInHistory :: X (Maybe Window)
nextInHistory = lastWindow <$> XS.get

doUpdateLastWindow :: X ()
doUpdateLastWindow = withWindowSet $ \ss -> XS.modify (updateLastWindow (W.peek ss))

greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s

selectWindowAnd initial action up down =
  do wset <- gets windowset
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
     rot initial
     repeatHintedKeys [ (up,   ("next", rot rotUp)) ,
                        (down, ("prev", rot rotDown)) ]

     withTaggedGlobal "S" $ delTag "S"
     ws <- io $ readIORef wref
         -- I want to swap the window for the focused instead
     action $ head ws

swapFocused choice =
  windows $ let replace old new l = flip map l $ \x -> if x == old then new else x
            in W.modify' $ \(W.Stack f u d) -> W.Stack choice
                                               (replace choice f u)
                                               (replace choice f d)

windowKeys = [ ("M-o", ("next", (focusUrgentOr focusLast)))

             , ("M-t", ("floaty",
                        withFocused $ \w -> do
                           isFloating <- gets (M.member w . W.floating . windowset)
                           if isFloating then windows $ W.sink w
                             else floatTo (0.6, 0.95) (0.05, 0.4) w
                         ))
             , ("M-.", ("swap selection", selectWindowAnd id swapFocused "M-." "M-,"))
             , ("M-,", ("swap selection", selectWindowAnd rotUp swapFocused "M-." "M-,"))
             , ("M-n", ("down", selectWindowAnd id (windows . W.focusWindow) "M-n" "M-p"))
             , ("M-p", ("up", selectWindowAnd rotUp (windows . W.focusWindow) "M-n" "M-p"))
             ]

focusNth n = windows $ foldr (.) W.focusMaster (Data.List.take n $ repeat W.focusDown)

focusLast = do (LastWindow _ lw) <- XS.get
               whenJust lw $ \lw' -> do
                 windows $ W.focusWindow lw'

focusUrgentOr a = do us <- readUrgents
                     if Data.List.null us then a else (focusUrgent >> warp)

addHistory c = c { logHook = doUpdateLastWindow >> (logHook c) }

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
