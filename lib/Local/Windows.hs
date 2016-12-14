 {-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, recentWindows, windowKeys, greedyFocusWindow) where

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
--import XMonad.Actions.GroupNavigation
import qualified XMonad.Util.ExtensibleState as XS
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Ratio

import qualified Debug.Trace as D

timeInSeconds :: IO Integer
timeInSeconds = round <$> getPOSIXTime

data RecentWindows = RW [(Integer, Window)]
  deriving (Typeable, Read, Show)

instance ExtensionClass RecentWindows where
  initialValue = RW []
  extensionType = PersistentExtension

debounce :: (Integer, Window) -> [(Integer, Window)] -> [(Integer, Window)]
debounce a [] = [a]
debounce (t, w) r@((t1, w1):tws)
  | w == w1 = (t, w):tws
  | t == t1 = (t, w):tws
  | otherwise = nubBy (\a b -> snd a == snd b) $ (t, w):r

recentWindows :: X [Window]
recentWindows = do
  RW hist <- XS.get
  -- we need to make sure that dead windows don't go in the list
  all <- (fmap W.allWindows (gets windowset))
  return $ nub $ (map snd hist) ++ all

lastFocus = do
  RW hist <- XS.get
  return $ case hist of
             _:((_, w):_) -> Just w
             _ -> Nothing

greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s

windowKeys = [ ("M-o", do
                   us <- readUrgents
                   l <- lastFocus
                   if null us then whenJust l $ (\l -> windows (greedyFocusWindow l))
                   else focusUrgent
                   warp)
             , ("M-k", kill)
             ]

addHistory c = c { logHook = updateHistory >> (logHook c) }

updateHistory :: X ()
updateHistory = do
  RW hist <- XS.get
  all <- (fmap W.allWindows (gets windowset))
  let clean = filter ((flip elem all) . snd)
  now <- io timeInSeconds
  focus <- gets (W.peek . windowset)
  whenJust focus $ \f'->
    let !newState = debounce (now, f') hist in
      XS.put $ RW $ clean $ newState

-- it would be nice to have my own history hook which somehow ignores the bogus focus events
-- produced by X.L.Groups
