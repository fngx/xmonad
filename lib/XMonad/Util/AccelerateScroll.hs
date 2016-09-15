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
          replicateM_ n $ XTest.fakeButtonPress dpy btn
          sync dpy False
          grabButton dpy btn 0 rootw False 0 grabModeAsync grabModeSync none none
  return ()
  where
    handle btn st@(ScrollEvents { lastButton = lb,  recent = ek }) now
      | btn /= lb = (ScrollEvents {lastButton = btn, recent = [now]}, 1)
      | otherwise =
        let ek' = trim (now:ek)
            trim (x:(y:xs)) = if (x - y) > 0.1 then [x] else x:(trim (y:xs))
            trim q = q in
          (st { recent = ek' }, min 10 $ floor $ sqrt $ fromIntegral $ length ek')

-- the last series of scroll events
data ScrollEvents = ScrollEvents
  {
    lastButton :: Button,
    recent :: [POSIXTime]
  }
  deriving (Typeable, Show)
instance ExtensionClass ScrollEvents where
  initialValue = ScrollEvents { lastButton = 0, recent = [] }
