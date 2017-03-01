{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Local.PerScreen (ifWider) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Maybe (fromMaybe)

import Control.Monad

-- like perscreen except works with tabs
-- can I do this with jumptolayout

data PerScreen l1 l2 a = PerScreen Dimension Bool (l1 a) (l2 a)
  deriving (Read, Show)

ifWider w = PerScreen w False

update _ Nothing Nothing = Nothing
update (PerScreen d b wl nl) mwl mnl = Just $ PerScreen d b (fromMaybe wl mwl) (fromMaybe nl mnl)

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) =>
  LayoutClass (PerScreen l1 l2) a where

  runLayout (W.Workspace i p@(PerScreen width wasWide wl nl) ms) r
    | rect_width r > width =
        do (wrs, mwl) <- runLayout (W.Workspace i wl ms) r
           mnl <- if wasWide then return Nothing else handleMessage nl (SomeMessage Hide)
           return (wrs, update p mwl mnl)
    | otherwise =
        do (wrs, mnl) <- runLayout (W.Workspace i nl ms) r
           mwl <- if not wasWide then return Nothing else handleMessage wl (SomeMessage Hide)
           return (wrs, update p mwl mnl)

  handleMessage (PerScreen w bool lt lf) m
    | bool      = handleMessage lt m >>= maybe (return Nothing) (\nt -> return . Just $ PerScreen w bool nt lf)
    | otherwise = handleMessage lf m >>= maybe (return Nothing) (\nf -> return . Just $ PerScreen w bool lt nf)

  description (PerScreen _ True  l1 _) = description l1
  description (PerScreen _ _ _ l2) = description l2
