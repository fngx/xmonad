module Local.SloppyFocus (sloppyFocus) where

import XMonad
import XMonad.Util.Timer
import Data.Monoid
import qualified XMonad.Util.ExtensibleState as XS


data SloppyFocus = SF (Maybe (TimerId, Window))

instance ExtensionClass SloppyFocus where
  initialValue = SF Nothing

sloppyFocus config =
  config
  { handleEventHook = (handleEventHook config) <+> focusFollow }

focusFollow e@(CrossingEvent {ev_window=w, ev_event_type=t})
  | t == enterNotify, ev_mode e == notifyNormal =
      do whenX (fmap not $ runQuery (className =? "XClock") w) $
           do t <- startTimer 0.1
              XS.put $ SF $ Just (t, w)
         return (All True)

focusFollow e = do
  (SF mw) <- XS.get
  whenJust mw $ \(ti, w) -> (handleTimer ti e (focus w >> return Nothing)) >> return ()
  return (All True)
