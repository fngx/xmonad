{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

module Local.Spacing (spacing) where

import Graphics.X11 (Rectangle(..), Position, Dimension)
import Control.Arrow (first, second)
import XMonad.Operations (sendMessage)
import XMonad.Core (X,runLayout,Message,fromMessage,Typeable)
import XMonad.StackSet (up, down, Workspace(..))
import XMonad.Util.Font (fi)

import XMonad.Layout.LayoutModifier
import qualified Debug.Trace as D

data Spacing a = Spacing { extra :: Int, inter :: Int }
  deriving (Read, Show)

-- since we are not informing the inner layout of the smaller box,
-- it cannot know which windows are edge windows any more
instance LayoutModifier Spacing a where
  modifyLayout (Spacing {extra = e, inter = i}) ws scr =
    let scr' = shrinkScreen e scr in
      first (map (second $ shrink i scr')) <$> runLayout ws scr'

shrinkScreen :: Int -> Rectangle -> Rectangle
shrinkScreen 0 r = r
shrinkScreen e (Rectangle sx sy sw sh) =
  Rectangle (fi (e + fi sx)) (fi (e + fi sy)) (fi $ (fi sw) - 2*e) (fi $ (fi sh) - 2*e)

shrink :: Int -> Rectangle -> Rectangle -> Rectangle
shrink 0 _ r = r
shrink i (Rectangle _sx _sy _sw _sh) (Rectangle _wx _wy _ww _wh) =
  Rectangle (fi nx :: Position) (fi ny :: Position) (fi nw :: Dimension) (fi nh :: Dimension)
  where
    (sx, sy, sw, sh) = (fi _sx, fi _sy, fi _sw, fi _sh)
    (wx, wy, ww, wh) = (fi _wx, fi _wy, fi _ww, fi _wh)

    nx = fix0 sx wx
    ny = fix0 sy wy

    nw = fix1 sx wx sw ww
    nh = fix1 sy wy sh wh

    fix0 ps pw | ps == pw  = pw
               | otherwise = pw + i

    fix1 ps pw ds dw | ds == dw           = dw
                     | pw + dw == ps + ds = max 1 $ dw - i
                     | ps == pw           = max 1 $ dw - i
                     | otherwise          = max 1 $ dw - 2*i

spacing e i = ModifiedLayout (Spacing e i)
