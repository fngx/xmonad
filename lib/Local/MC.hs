{-# LANGUAGE FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, ScopedTypeVariables #-}

module Local.MC where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import XMonad.Layout (splitHorizontallyBy, splitVerticallyBy)
import Data.Maybe
import Data.List (intercalate, transpose)
import Control.Arrow (first, second, (&&&))
import Control.Applicative ((<$>))
import qualified Data.Map.Strict as M
import qualified Debug.Trace as D
import XMonad.Util.Types (Direction2D (..))
import Graphics.X11.Xlib.Extras (getWindowAttributes,
                                 WindowAttributes (..))

import Graphics.X11.Xlib.Misc (warpPointer)
import XMonad.Actions.RotSlaves (rotAll')

data MC l a = MC
  { cells :: [(Rational, [Rational])]
  , lastCells :: [(Rational, [Rational])]
  , overflow :: l a
  , coords :: M.Map a (Int, Int)
  , mirror :: Bool
  , lastRect :: Rectangle
  , overflowFocus :: Int
  } deriving (Read, Show)

mc :: l a -> [(Rational, [Rational])] -> MC l a
mc il c0 = MC { cells = c0
              , overflow = il
              , coords = M.empty
              , mirror = False
              , lastCells = []
              , lastRect = Rectangle 0 0 10 10
              , overflowFocus = 0 }

data MCMsg a =
  SetCells [(Rational, [Rational])] |
  ResizeCell Rational Rational a |
  SetEdge Direction2D Position a |
  ChangeCells (Maybe (Int, Int) -> [(Rational, [Rational])] -> [(Rational, [Rational])]) a |
  Flip |
  OnOverflow ([Window] -> [Window])

instance Typeable a => Message (MCMsg a)

flipR :: Rectangle -> Rectangle
flipR (Rectangle sx sy sw sh) = Rectangle sy sx sh sw

a /? 0 = a
a /? b = a / b

instance (Typeable a, Ord a, Show a, LayoutClass l a) => LayoutClass (MC l) a where
  description (MC { cells = cs, mirror = m }) =
    concat [ if m then "M" else ""
           , intercalate "|" (map (show . length . snd) cs) ]

  runLayout wspa@(W.Workspace _ state stack) rect' = do
    let mirr = if mirror state then flipR else id
        rect = mirr rect'
        ws = W.integrate' stack
        capacity = foldl (+) 0 $ map (length . snd) $ cells state
        demand = length ws

        cutV (Rectangle sx sy sw sh) (l, r) =
          let wx = sx + ((round $ (l * fromIntegral sw)) :: Position)
              wr = sx + ((round $ (r * fromIntegral sw)) :: Position)
              ww = (fromIntegral wr - fromIntegral wx) :: Dimension
          in Rectangle wx sy ww sh
        cutH r bs = flipR $ cutV (flipR r) bs

        explode cut rect rats =
          let t = sum rats
              parts = map (flip (/?) t) $ scanl (+) 0 rats
              bounds = zip parts (drop 1 parts)
          in map (cut rect) bounds

        limit :: Int -> [(Rational, [Rational])] -> [(Rational, [Rational])]
        limit 0 _ = []
        limit n ((cw, rws):rest)
          | length rws >= n = [(cw, take n rws)]
          | otherwise = (cw, rws):(limit (n - length rws) rest)

        divide :: [(Rational, [Rational])] -> [(Rectangle, (Int, Int))]
        divide cs = let cols = explode cutV rect $ map fst cs
                        rows = map (uncurry (explode cutH)) $ zip cols $ map snd cs
                   in concatMap (\(c, rs) -> zip rs (map ((,) c) [0 :: Int ..])) $ zip [0 :: Int ..] rows

    if capacity >= demand
      then do let cs = limit demand $ cells state
                  rs = divide cs
                  rects = map fst rs
              o' <- handleMessage (overflow state) $ SomeMessage Hide
              let state' = state { overflow = fromMaybe (overflow state) o'
                                 , coords = M.fromList (zip ws $ map snd rs)
                                 , lastCells = cs
                                 , lastRect = rect
                                 , overflowFocus = 0 }
              return $ (zip ws (map mirr rects), Just $ state')
      else do let rs = divide $ cells state
                  rects = map fst rs
                  (main, extra) = splitAt (capacity - 1) ws
                  fIndex = ((maybe 0 (length . W.up) stack) - (capacity - 1))
                  odex
                    | fIndex < 0 = (overflowFocus state) `mod` (length extra)
                    | otherwise = fIndex
                  ostack = fromIndex extra odex
                  orect = last rects
                  ocoord = (length (cells state) - 1,
                            length (snd $ last $ cells state) - 1)
              (owrs, o') <- runLayout wspa { W.stack = ostack
                                           , W.layout = (overflow state) } (mirr orect)
              let state' = state { overflow = fromMaybe (overflow state) o'
                                 , coords = M.fromList (zip main $ map snd rs) `M.union` M.fromList (zip extra $ repeat ocoord)
                                 , lastCells = (cells state)
                                 , lastRect = rect
                                 , overflowFocus = odex }
              return $ ((zip main (map mirr rects)) ++ owrs, Just $ state')

  handleMessage state sm
    | Just (ButtonEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (PropertyEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (ExposeEvent {}) <- fromMessage sm = overflowHandle state sm
    | Just (Hide) <- fromMessage sm = overflowHandle state sm
    | Just (ReleaseResources) <- fromMessage sm = overflowHandle state sm
    | Just (SetCells cs :: MCMsg a) <- fromMessage sm = if (cells state) /= cs
                                             then return $ Just $ state { cells = cs }
                                             else return $ Nothing
    | Just (ResizeCell dx dy a) <- fromMessage sm =
        let (dx', dy') = if mirror state then (dy, dx) else (dx, dy) in
          return $ (resizeCell (normalizeState state) (+ dx') (+ dy')) <$> (M.lookup a $ coords state)

    | Just (SetEdge e pos w) <- fromMessage sm =
        let lr = (if mirror state then flipR else id) (lastRect state)
            p :: Rational
            p = if e == L || e == R
                then (fromIntegral $ pos - (fromIntegral $ rect_x $ lr)) /?
                     (fromIntegral $ rect_width $ lr)
                else (fromIntegral $ pos - (fromIntegral $ rect_y $ lr)) /?
                     (fromIntegral $ rect_height $ lr)
        in return $ (setEdgeAbsolute (normalizeState state) (unmirror e) p) <$> (M.lookup w $ coords state)

    | Just (ChangeCells f w :: MCMsg a) <- fromMessage sm =
        return $ Just $ state { cells = f (M.lookup w $ coords state) (cells state) }

    | Just (Flip :: MCMsg a) <- fromMessage sm = return $ Just $ state { mirror = not (mirror state) }

    | Just (OnOverflow f :: MCMsg a) <- fromMessage sm = do
        let capacity = (foldl (+) 0 $ map (length . snd) $ cells state) - 1
        -- rearrange some of the windows
        windows $ W.modify' (rotAll' $ \l -> let (u, d) = splitAt capacity l
                                           in u ++ f d)
        return Nothing

    | otherwise = return Nothing
    where unmirror e
            | mirror state = case e of
                               L -> U
                               R -> D
                               U -> L
                               D -> R
            | otherwise = e
overflowHandle state sm = do o' <- handleMessage (overflow state) sm
                             return $ fmap (\x -> state {overflow = x}) o'

normalizeState state =
  let cells0 = cells state
      cells1 = lastCells state
      ctotal = sum $ map fst cells1
      rtotals = (map (sum . snd) cells1) ++ repeat 0
  in state { cells =
             flip map (zip cells0 rtotals) $
             \((c, rs), rtotal) -> (c /? ctotal, map (flip (/?) rtotal) rs)
           }

setAbsolute _ _ _ [] = []
setAbsolute 0 p psf (t:(n:rs)) =
  let t' = min (n + t - 0.05) $ max 0.05 (p - psf)
      n' = n + (t - t')
  in t':n':rs
setAbsolute 0 p psf (t:[]) = [t]
setAbsolute n p psf (x:xs) = x:(setAbsolute (n - 1) p (psf + x) xs)

setEdgeAbsolute :: MC l a -> Direction2D -> Rational -> (Int, Int) -> MC l a
setEdgeAbsolute state e p (c, r)
  | c < 0 = state
  | r < 0 = state
  | e == L = setEdgeAbsolute state R p (c - 1, r)
  | e == U = setEdgeAbsolute state D p (c, r - 1)
  | e == R = let cps = setAbsolute c p 0 $ (map fst $ cells state)
             in state { cells = zip cps $ map snd $ cells state }
  | e == D = state { cells = toNth (second $ (setAbsolute r p 0)) c (cells state) }
  | otherwise = state

resizeCell state tx ty (ci, ri) = state
  { cells = toNth (second (toNth ty ri) . first tx) ci (cells state) }

toNth _ _ [] = []
toNth f 0 (x:xs) = (f x):xs
toNth f n (x:xs) = x:(toNth f (n-1) xs)


-- This has a bug in that it uses the whole screen rect when the
-- layout sees a reduced rect because of e.g. struts
mouseResizeTile :: Rational -> (Window -> X ())  -> Window -> X ()
mouseResizeTile border fallback w =
  whenX (isClient w) $
  withDisplay $ \dpy -> do
  wa <- io $ getWindowAttributes dpy w
  (_, _, _, ox', oy', _, _, _) <- io $ queryPointer dpy w

  let wx = fromIntegral $ wa_x wa
      wy = fromIntegral $ wa_y wa
      ww = fromIntegral $ wa_width wa
      wh = fromIntegral $ wa_height wa
      ox = fromIntegral ox'
      oy = fromIntegral oy'

      drag mouse wpos wdim e1 e2
        | mouse - wpos < (wdim * border) =
            (True, 0, \px -> sendMessage $ SetEdge e1 px w)
        | (wpos + wdim) - mouse < (wdim * border) =
            (True, wdim, \px -> sendMessage $ SetEdge e2 px w)
        | otherwise =
            (False, mouse - wpos, \px -> return ())

      (hitX, warpx, dragX) = drag ox wx ww L R
      (hitY, warpy, dragY) = drag oy wy wh U D

      dragHandler x y = do
        dragX x
        dragY y
      stopHandler = return ()
  if hitX || hitY
    -- warp pointer here
    then do io $ warpPointer dpy none w 0 0 0 0 (floor warpx) (floor warpy)
            mouseDrag dragHandler stopHandler
    else fallback w
