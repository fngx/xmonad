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
    Visible -> do sendMessage $ SetStruts [U, D] []
    Hidden -> do sendMessage $ SetStruts [] [U, D]
    _ -> return ()

toggleBarHook :: Event -> X All
toggleBarHook e = do
  st <- XS.get
  case st of
    (Temporary ti) -> handleTimer ti e $
      do XS.put Hidden
         return Nothing
    _ -> return Nothing
  resetBar
  return (All True)
