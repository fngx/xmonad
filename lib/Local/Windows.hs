 {-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, recentWindows, windowKeys, greedyFocusWindow) where

import Local.Marks

import Control.Applicative ((<$>))
import Control.Monad
import Data.List
import Data.Maybe
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

import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Hooks.FloatNext (runLogHook)

import qualified Debug.Trace as D

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

windowKeys = [ ("M-o", ("last focus", prevFocus))
             , ("M-i", ("next focus", nextFocus))
             ]

focusUrgentOr a = do us <- readUrgents
                     if Data.List.null us then a else (focusUrgent >> warp)

nextInHistory m = recentWindows >>=
                  (return . (Data.List.drop 1)) >>=
                  (if m then marked 'o' else unmarked 'o') >>=
                  (return . listToMaybe)

nextFocus = focusUrgentOr $
  do nih <- nextInHistory False
     whenJust nih $ \h -> do withFocused $ mark 'o'
                             windows $ W.focusWindow h
     warp

prevFocus = do nih <- nextInHistory True
               whenJust nih $ \h -> do withFocused $ unmark 'o'
                                       windows $ W.focusWindow h
               warp

addHistory c = c { logHook = hook >> (logHook c) }

hook :: X ()
hook = XS.get >>= updateHistory >>= XS.put

getWindowRect :: Window -> X Rectangle
getWindowRect w = do d <- asks display
                     atts <- io $ getWindowAttributes d w
                     let bw = wa_border_width atts
                     return $ Rectangle
                       (fromIntegral $ wa_x atts)
                       (fromIntegral $ wa_y atts)
                       (fromIntegral $ bw + wa_width atts)
                       (fromIntegral $ bw + wa_height atts)
