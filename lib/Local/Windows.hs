 {-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, recentWindows, windowKeys, greedyFocusWindow, nextInHistory) where

import Local.Marks

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
import XMonad.Util.NamedWindows (getName)
import Data.Monoid

import XMonad.Util.XUtils
import XMonad.Util.Font

data WindowHistory = WH (Maybe Window) (Seq Window)
  deriving (Typeable, Read, Show)

instance ExtensionClass WindowHistory where
  initialValue = WH Nothing empty
  extensionType = PersistentExtension

updateHistory :: WindowHistory -> X WindowHistory
updateHistory (WH oldcur oldhist) = withWindowSet $ \ss -> do
  let newcur   = W.peek ss
      wins     = Set.fromList $ W.allWindows ss
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

windowKeys = [ ("M-o",   ("flip",       focusUrgentOr $ focusDotOr $
                                        ((listToMaybe . Data.List.drop 1) <$> recentWindows) >>=
                                        (flip whenJust (windows . W.focusWindow))))

             , ("M-S-o", ("last focus", do whenX (Data.List.null <$> allMarked '.') $
                                             withFocused $ mark '.'
                                           nextFocus))

             , ("M-t", ("floaty",
                        withFocused $ \w -> do
                           isFloating <- gets (M.member w . W.floating . windowset)
                           if isFloating then windows $ W.sink w
                             else floatTo (0.6, 0.95) (0.05, 0.4) w
                         ))
             ]

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

focusDotOr a = do m <- allMarked '.'
                  if Data.List.null m then a
                    else do unmark '.' (head m)
                            windows $ W.focusWindow $ head m
                  clearMarks 'o'

focusUrgentOr a = do us <- readUrgents
                     if Data.List.null us then a else (focusUrgent >> warp)

nextInHistory = recentWindows >>=
                  (return . (Data.List.drop 1)) >>=
                  (unmarked 'o') >>=
                  (return . listToMaybe)

nextFocus = focusUrgentOr $
  do nih <- nextInHistory
     case nih of
       (Just h) -> do withFocused $ mark 'o'
                      windows $ W.focusWindow h
       Nothing -> do clearMarks 'o'
                     nextFocus
     warp

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
