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
import qualified Local.Theme as Theme
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import XMonad.Util.NamedWindows (getName, unName)
import Data.Monoid
import Local.Hints (repeatHintedKeys)

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

windowKeys = [ ("M-o", ("next", do oldFocus <- gets (W.peek . windowset)
                                   focusUrgentOr focusNext
                                   repeatHintedKeys [("M-o", ("next", focusNext)) ,("M-i", ("prev", focusPrev))]
                                   withTagged "." $ delTag "."
                                   -- push start window down to next focus
                                   XS.get >>= insertHistory oldFocus >>= updateHistory >>= XS.put
                       ))

             , ("M-t", ("floaty",
                        withFocused $ \w -> do
                           isFloating <- gets (M.member w . W.floating . windowset)
                           if isFloating then windows $ W.sink w
                             else floatTo (0.6, 0.95) (0.05, 0.4) w
                         ))
             ]

focusUrgentOr a = do us <- readUrgents
                     if Data.List.null us then a else (focusUrgent >> warp)

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
