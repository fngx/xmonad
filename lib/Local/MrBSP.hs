module Local.MrBSP where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Types

data BSPLayout a = BSPLayout { tree :: BSP }

data BSP =
  Leaf |
  Branch { ratio :: Rational
         , split :: Direction1D
         , left  :: BSP
         , right :: BSP }
  deriving (Read, Show, Typeable)

type Path = [Direction1D]

allocate :: BSP -> Path -> Rectangle -> [a] -> ([(Path, a, Rectangle)], [a])
allocate _ p r [] = []
allocate Leaf p r (a:as) = ([(p, a, r)], as)
allocate _ p r (a:[]) = ([(p, a, r)], [])
allocate b@(Branch {}) p r as =
  let (Rectangle x y w h) = r
      (leftR, rightR) = case split b of
        Next -> (Rectangle x y w/2 h, Rectangle x+w/2 y w/2 h)
        Prev -> (Rectangle x y w h/2, Rectangle x y+h/2 w h/2)
      (leftAlloc, leftLeft) = allocate (left b) (Prev:p) leftR as
      (rightAlloc, rightLeft) = allocate (right b) (Prev:p) rightR leftLeft
  in (leftAlloc ++ rightAlloc, rightLeft)

instance LayoutClass BSPLayout a where
  doLayout state rect stack = do
    let wins = W.integrate' stack
        (cells, excess) = allocate (tree state) [] rect wins
    -- if excess is null then destroy the tabs and return rects
    -- otherwise, we need to combinorize the tabs with what is beneath them.
