{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, MultiParamTypeClasses #-}

module XMonad.Layout.ToggleLimit where

import XMonad.Layout.MultiToggle
import qualified XMonad.Layout.LimitWindows as Limit
import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Renamed

data ToggleLimit = TL Int
  deriving (Read, Show, Eq, Typeable)

instance Transformer ToggleLimit Window where
  transform (TL n) x k = k (renamed [Prepend ("Limit " ++ (show n) ++ " ")] (Limit.limitWindows n x))
    (\(ModifiedLayout _ (ModifiedLayout _ x')) -> x')
