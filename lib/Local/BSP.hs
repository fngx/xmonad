{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Local.BSP where

import XMonad
import XMonad.Layout (splitHorizontallyBy, splitVerticallyBy)
import qualified XMonad.StackSet as W
import XMonad.Util.Types
import qualified Data.Map.Strict as M
import Control.Arrow ((&&&))

data BSPLayout a = BSPLayout { tree :: BSP
                             , paths :: M.Map a Path }
  deriving (Read, Show, Typeable)

data BSP =
  Leaf |
  Branch { ratio :: Rational
         , split :: Direction1D
         , left  :: BSP
         , right :: BSP }
  deriving (Read, Show, Typeable)

data BSPMessage a = SplitH a | SplitV a
  deriving (Typeable)

instance (Typeable a) => Message (BSPMessage a)

type Path = [Direction1D]

capacity :: BSP -> Int
capacity Leaf = 1
capacity (Branch {left = l, right = r}) = capacity l + capacity r

selectN :: Int -> BSP -> (BSP, Int)
selectN 1 _ = (Leaf, 0)
selectN n Leaf = (Leaf, n-1)
selectN n b@(Branch {left = l, right = r}) =
  let (selectL, nL) = selectN n l
      (selectR, nR) = selectN nL r
  in if nL > 0
     then (b { left = selectL, right = selectR }, nR)
     else (selectL, 0)

cutup :: BSP -> Path -> Rectangle -> [(Path, Rectangle)]
cutup Leaf p r = [(p, r)]
cutup (Branch {left = le, right = ri, split = sp, ratio = ra}) p r =
  let splitr = case sp of
        Next -> splitHorizontallyBy
        Prev -> splitVerticallyBy
      (leftR, rightR) = splitr ra r
      lcut = cutup le (Prev:p) leftR
      rcut = cutup ri (Next:p) rightR
  in lcut ++ rcut

describe :: BSP -> String
describe Leaf = "*"
describe (Branch {split = s, left = l, right = r}) =
  de s ++ "("++describe l  ++","++ describe r++")"
  where de Prev = "V"
        de Next = "H"

instance (Typeable a, Show a, Ord a) => LayoutClass BSPLayout a where
  description state = describe $ tree state

  doLayout state rect stack = do
    let wins = W.integrate stack
        (tree', excessN) = selectN (length wins) (tree state)
        cells = cutup tree' [] rect
        allPaths = map fst cells ++ (repeat $ fst $ last cells)
        newPaths = M.fromList $ zip wins allPaths
        wrs = zip wins $ map snd cells

    return $ (wrs, Just $ state { paths = newPaths })
    -- if null excess
    --   then return $ (map snd cells, Nothing) -- Tabbed layout
    --   else return $ (map snd cells, Nothing)

  handleMessage state sm = case fromMessage sm of
    Just (SplitH w) -> return $ Just $ branch Prev w state
    Just (SplitV w) -> return $ Just $ branch Next w state
    Nothing -> return Nothing

bsp = BSPLayout { tree = Branch { ratio = 0.5
                                , split = Next
                                , left = Leaf
                                , right = Leaf }
                , paths = M.empty }

editBSP :: (BSP -> BSP) -> Path -> BSP -> BSP
editBSP f [] b = f b
editBSP _ (p:ps) Leaf =  Leaf
editBSP f (Prev:ps) b@(Branch {left = l})  = b { left  = editBSP f ps l }
editBSP f (Next:ps) b@(Branch {right = l}) = b { right = editBSP f ps l }

branch :: (Ord a) => Direction1D -> a -> BSPLayout a -> BSPLayout a
branch d w st@(BSPLayout {paths = ps, tree = tr}) =
  maybe st withSplitAt $ M.lookup w ps
  where withSplitAt path = st { tree = editBSP insertBranch path (tree st) }
        insertBranch Leaf = Branch { ratio = 0.5, split = d, left = Leaf, right = Leaf }
        insertBranch x = x
