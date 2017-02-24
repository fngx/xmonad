{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, TypeFamilies #-}

module Local.MC where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import XMonad.Layout (splitHorizontallyBy, splitVerticallyBy)
import Data.Maybe
import Data.List (intercalate)
import qualified Debug.Trace as D

-- TODO: remember overflow focus?
-- TODO: messages for border resize and grow / shrink / etc
-- TODO: add gaps?

data MC l a = MC
  { cells :: [(Rational, [Rational])]
  , overflow :: l a
  } deriving (Read, Show)

data MCMsg = SetCells [(Rational, [Rational])]

instance Message MCMsg

instance (Show a, LayoutClass l a) => LayoutClass (MC l) a where
  description (MC { cells = cs }) =
    intercalate "|" (map (show . length . snd) cs)

  runLayout wspa@(W.Workspace _ state stack) rect = do
    let ws = W.integrate' stack
        capacity = foldl (+) 0 $ map (length . snd) $ cells state
        demand = length ws

        cutV (Rectangle sx sy sw sh) (l, r) =
          (Rectangle (pos l sx sw) sy (ext l r sw) sh )
        cutH (Rectangle sx sy sw sh) (l, r) =
          (Rectangle sx (pos l sy sh) sw (ext l r sh))

        pos l p h = (ceiling ((fromIntegral p) + (fromIntegral h) * l) :: Position)
        ext l r h = (floor ((fromIntegral h) * (r - l)) :: Dimension)

        explode cut rect rats =
          let t = sum rats
              parts = scanl (+) 0 $ map (flip (/) t) rats
              bounds = zip parts (drop 1 parts)
          in map (cut rect) bounds

        limit :: Int -> [(a, [b])] -> [(a, [b])]
        limit 0 [] = []
        limit n ((cw, rws):rest)
          | length rws >= n = [(cw, take n rws)]
          | otherwise = (cw, rws):(limit (n - length rws) rest)

        divide :: Int -> [(Rectangle, (Int, Int))]
        divide n = let cs = limit n $ cells state
                       cols = explode cutV rect $ map fst cs
                       rows = map (uncurry (explode cutH)) $ zip cols $ map snd cs
                   in concatMap (\(c, rs) -> zip rs (map ((,) c) [0 :: Int ..])) $ zip [0 :: Int ..] rows

    if capacity >= demand
      then do let rs = divide demand
                  rects = map fst rs
              o' <- handleMessage (overflow state) $ SomeMessage Hide
              let state' = state { overflow = fromMaybe (overflow state) o' }
              return $ (zip ws rects, Just $ state')
      else do let rs = divide capacity
                  rects = map fst rs
                  (main, extra) = splitAt (capacity - 1) ws
                  odex = (D.traceShowId $ maybe 0 (length . W.up) stack) - (capacity-1)
                  ostack = fromIndex extra odex
              (owrs, o') <- runLayout wspa { W.stack = ostack
                                           , W.layout = (overflow state) } (last rects)
              let state' = state { overflow = fromMaybe (overflow state) o' }
              return $ ((zip main rects) ++ owrs, Just $ state')

  handleMessage state sm
    | Just (ButtonEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (PropertyEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (ExposeEvent {}) <- fromMessage sm = overflowHandle state sm

  handleMessage state sm
    | Just Hide <- fromMessage sm = overflowHandle state sm
    | Just ReleaseResources <- fromMessage sm = overflowHandle state sm

  handleMessage state sm = case fromMessage sm of
    Just (SetCells cs) -> if (cells state) /= cs
                          then return $ Just $ state { cells = cs }
                          else return $ Nothing
    _ -> return Nothing

  handleMessage state sm = return Nothing

overflowHandle state sm = do o' <- handleMessage (overflow state) sm
                             return $ fmap (\x -> state {overflow = x}) o'
