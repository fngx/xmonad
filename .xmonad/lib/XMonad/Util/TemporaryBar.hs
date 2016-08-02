{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Util.TemporaryBar where

import XMonad
import XMonad.Hooks.ManageDocks (SetStruts (..))
import XMonad.Util.Types
import Data.Bits
import Data.Monoid
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Timer
import Control.Monad

data BarVis = Visible | Hidden | Temporary TimerId
  deriving (Typeable, Show)

instance ExtensionClass BarVis where
  initialValue = Visible

toggleBar = do
  XS.modify $ \x -> case x of
    Visible -> Hidden
    Hidden -> Visible
    (Temporary _) -> Visible
  resetBar

tempShowBar time = do
  sendMessage $ SetStruts [U, D] []
  st <- XS.get
  case st of
    Visible -> return ()
    _ -> do t <- startTimer time
            XS.put $ Temporary t
  resetBar

resetBar = do
  st <- XS.get
  case st of
    Visible -> sendMessage $ SetStruts [U, D] []
    Hidden -> sendMessage $ SetStruts [] [U, D]
    _ -> return ()

-- ungrab :: X ()
-- ungrab = do XConf {theRoot = root, display = d} <- ask
--             io $ ungrabKeyboard d currentTime
--             resetBar

toggleBarHook :: Event -> X All
-- toggleBarHook (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = c})
--   | t == keyPress && (m .&. mod4Mask /= 0) =
--     do XConf {theRoot = root, display = d} <- ask
--        setBar True
--        io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
--        return (All True)
--   | t == keyRelease =
--     do XConf {theRoot = root, display = d} <- ask
--        s <- io $ keycodeToKeysym d c 0
--        if s == xK_Super_L then do
--          setBar False
--          io $ ungrabKeyboard d currentTime
--          return (All True)
--          else return (All True)

toggleBarHook e = do
  st <- XS.get
  case st of
    (Temporary ti) -> handleTimer ti e $
      do XS.put Hidden
         return Nothing
    _ -> return Nothing
  resetBar
  return (All True)
