{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Local.PerScreen (ifWider) where

import XMonad
import qualified XMonad.StackSet as W
import Control.Applicative ((<$>))

import Data.Maybe (fromMaybe)

import Control.Monad

-- like perscreen except works with tabs
-- can I do this with jumptolayout

data PerScreen wlt nlt a = PerScreen { width :: Dimension
                                     , isWide :: Bool
                                     , wide :: (wlt a)
                                     , narrow :: (nlt a)}
  deriving (Read, Show)

ifWider w = PerScreen w False

update _ Nothing Nothing Nothing = Nothing
update st@(PerScreen { isWide = b, wide = wl, narrow = nl }) mb mwl mnl =
  Just $ st { isWide = fromMaybe b mb
            , wide = fromMaybe wl mwl
            , narrow = fromMaybe nl mnl }

toggle a b
  | a == b = Nothing
  | otherwise = Just a

instance (LayoutClass l1 a, LayoutClass l2 a, Show a) =>
  LayoutClass (PerScreen l1 l2) a where

  runLayout (W.Workspace i st ms) r
    | rect_width r > width st =
        do (wrs, mwl) <- runLayout (W.Workspace i (wide st) ms) r
           mnl <- if (isWide st) then return Nothing else handleMessage (narrow st) (SomeMessage Hide)
           return (wrs, update st (toggle True (isWide st)) mwl mnl)
    | otherwise =
        do (wrs, mnl) <- runLayout (W.Workspace i (narrow st) ms) r
           mwl <- if not (isWide st) then return Nothing else handleMessage (wide st) (SomeMessage Hide)
           return (wrs, update st (toggle False (isWide st)) mwl mnl)

  handleMessage st m
    | isWide st = (handleMessage (wide st) m)   >>= (return . (fmap $ \nw -> st { wide = nw }))
    | otherwise = (handleMessage (narrow st) m) >>= (return . (fmap $ \nw -> st { narrow = nw }))

  description st
    | isWide st = description (wide st)
    | otherwise = description (narrow st)
