 {-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, recentWindows, windowKeys, greedyFocusWindow, lastFocus) where

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
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Ratio

import qualified Debug.Trace as D

timeInSeconds :: IO Integer
timeInSeconds = round <$> getPOSIXTime

type FTime = Integer

data RecentWindows = RW { _lastFocus :: FTime, _lastWindow :: Window, _history :: [Window] }
  deriving (Typeable, Read, Show)

instance ExtensionClass RecentWindows where
  initialValue = RW 0 0 []
  extensionType = PersistentExtension

updateFocus :: FTime -> Window -> RecentWindows -> RecentWindows
updateFocus t w r@(RW {_lastFocus = t1, _lastWindow = w1, _history = h})
  | w == w1 = r { _lastFocus = t }
  | t == t1 && w `elem` h = r { _lastFocus = t1, _lastWindow = w }
  | t == t1 = r { _lastFocus = t1, _lastWindow = w, _history = w1:h }
  | otherwise = r { _lastFocus = t1, _lastWindow = w, _history = nub $ w1:h }

cleanup :: [Window] -> RecentWindows -> RecentWindows
cleanup ws r@(RW {_history = h}) = let good = flip elem ws in r { _history = filter good h }

recentWindows :: X [Window]
recentWindows = do
  (RW {_lastWindow = w, _history = h}) <- XS.get
  all <- (fmap W.allWindows (gets windowset))
  let clean = filter (flip elem all)
  return $ nub $ (clean $ w:(delete w h))++all

lastFocus = do
  rws' <- recentWindows
  all <- (fmap W.allWindows (gets windowset))
  let clean = filter (flip elem all)
      rws = clean rws'
  return $ case rws of
             (h:t) -> listToMaybe $ delete h t
             _ -> Nothing

greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s

windowKeys = [ ("M-o", ("last focus", lastWindow))
             , ("M-k", ("kill", kill))
             ]

lastWindow = do us <- readUrgents
                l <- lastFocus
                if null us then whenJust l $ (windows . W.focusWindow)
                  else focusUrgent
                warp

addHistory c = c { logHook = updateHistory >> (logHook c) }

updateHistory :: X ()
updateHistory = do
  all <- (fmap W.allWindows (gets windowset))
  now <- io timeInSeconds
  focus <- gets (W.peek . windowset)

  whenJust focus $ \focus -> XS.modify (updateFocus now focus)
  XS.modify (cleanup all)

-- it would be nice to have my own history hook which somehow ignores the bogus focus events
-- produced by X.L.Groups
