{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Util.AccelerateScroll where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)

import qualified Graphics.X11.XTest as XTest

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Monoid

import qualified Debug.Trace as DT

accelerateButton btn = do
  dpy <- asks display
  rootw <- asks theRoot
  now <- io getPOSIXTime
  st <- XS.get
  let (state', n) = handle btn st now
  XS.put state'
  io $ do ungrabButton dpy btn 0 rootw
          sync dpy False
          replicateM_ n $ XTest.fakeButtonPress dpy btn >> threadDelay 5
          sync dpy False
          grabButton dpy btn 0 rootw False 0 grabModeAsync grabModeSync none none
  return ()
  where
    handle btn st@(ScrollEvents { lastButton = lb,  recent = ek }) now
      | btn /= lb = (ScrollEvents {lastButton = btn, recent = [now]}, 0)
      | otherwise = let times' = now:(filter (\x -> now - x < 0.5) ek)
                        rate = (fromIntegral $ length times')
                        n = 1+(floor ((rate/2) ** 1.25)) in
          (st { recent = times' }, n)

-- the last series of scroll events
data ScrollEvents = ScrollEvents
  {
    lastButton :: Button,
    recent :: [POSIXTime]
  }
  deriving (Typeable, Show)
instance ExtensionClass ScrollEvents where
  initialValue = ScrollEvents { lastButton = 0, recent = [] }
