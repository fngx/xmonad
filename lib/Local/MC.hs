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

-- TODO: remember overflow focus / rearrange overflow focus automatically
-- TODO: messages for border resize and grow / shrink / etc
-- TODO: add gaps?
-- TODO: small screen layout swapper?

data MC l a = MC
  { cells :: [(Rational, [Rational])]
  , overflow :: l a
  , coords :: M.Map a (Int, Int)
  , mirror :: Bool
  , fillColumns :: Bool
  } deriving (Read, Show)

mc :: l a -> [(Rational, [Rational])] -> MC l a
mc il c0 = MC { cells = c0 , overflow = il, coords = M.empty, mirror = False }

data MCMsg a =
  SetCells [(Rational, [Rational])] |
  ResizeCell Rational Rational a |
  SetEdge Direction2D Rational a |
  ChangeCells (Maybe (Int, Int) -> [(Rational, [Rational])] -> [(Rational, [Rational])]) a |
  Flip

instance Typeable a => Message (MCMsg a)

instance (Typeable a, Ord a, Show a, LayoutClass l a) => LayoutClass (MC l) a where
  description (MC { cells = cs, mirror = m, fillColumns = fc }) =
    concat [ if mirror then "M" else ""
           , if fillColumns then "C" else ""
           , intercalate "|" (map (show . length . snd) cs) ]

  runLayout wspa@(W.Workspace _ state stack) rect' = do
    let mirr r@(Rectangle sx sy sw sh)
          | mirror state = (Rectangle sy sx sh sw)
          | otherwise = r
        rect = mirr rect'
        ws = W.integrate' stack
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

        limit :: Int -> [(Rational, [Rational])] -> [(Rational, [Rational])]
        limit 0 _ = []
        limit n ((cw, rws):rest)
          | length rws >= n = [(cw, take n rws)]
          | otherwise = (cw, rws):(limit (n - length rws) rest)

        divide :: Int -> [(Rectangle, (Int, Int))]
        divide n = let cs :: [(Rational, [Rational])]
                       cs = limit n $ cells state
                       cols = explode cutV rect $ map fst cs
                       rows = map (uncurry (explode cutH)) $ zip cols $ map snd cs
                   in concatMap (\(c, rs) -> zip rs (map ((,) c) [0 :: Int ..])) $ zip [0 :: Int ..] rows

    if capacity >= demand
      then do let rs = divide demand
                  rects = map fst rs
              o' <- handleMessage (overflow state) $ SomeMessage Hide
              let state' = state { overflow = fromMaybe (overflow state) o'
                                 , coords = M.fromList (zip ws $ map snd rs)}
              return $ (zip ws (map mirr rects), Just $ state')
      else do let rs = divide capacity
                  rects = map fst rs
                  (main, extra) = splitAt (capacity - 1) ws
                  odex = (maybe 0 (length . W.up) stack) - (capacity-1)
                  ostack = fromIndex extra odex
                  orect = last rects
                  ocoord = (length (cells state) - 1,
                            length (snd $ last $ cells state) - 1)
              (owrs, o') <- runLayout wspa { W.stack = ostack
                                           , W.layout = (overflow state) } (mirr orect)
              let state' = state { overflow = fromMaybe (overflow state) o'
                                 , coords = M.fromList (zip main $ map snd rs) `M.union` M.fromList (zip extra $ repeat ocoord) }
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

    | Just (SetEdge e p w) <- fromMessage sm =
        return $ (setEdgeAbsolute state (unmirror e) p) <$> (M.lookup w $ coords state)

    | Just (ChangeCells f w :: MCMsg a) <- fromMessage sm =
        return $ Just $ state { cells = f (M.lookup w $ coords state) (cells state) }

    | Just (Flip :: MCMsg a) <- fromMessage sm = return $ Just $ state { mirror = not (mirror state) }

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
  let cells0 = cells state in
    state { cells =
            (zip
              (normalize $ map fst cells0)
              (map normalize $ map snd cells0)) }

normalize a = let s = sum a in map (flip (/) s) a

setAbsolute _ _ _ [] = []
setAbsolute 0 p psf (t:(n:rs)) =
  -- n + (t - t') > 0.05
  -- n + t > 0.05 + t'
  -- t' > n + t - 0.05
  let t' = min (n + t - 0.05) $ max 0.05 (p - psf)
      n' = n + (t - t')
  in t':n':rs
setAbsolute 0 p psf (t:[]) = [t]
setAbsolute n p psf (x:xs) = x:(setAbsolute n p (psf + x) xs)

setEdgeAbsolute :: MC l a -> Direction2D -> Rational -> (Int, Int) -> MC l a
setEdgeAbsolute state e p (c, r)
  | c < 0 = state
  | r < 0 = state
  | e == L = setEdgeAbsolute state R p (c - 1, r)
  | e == U = setEdgeAbsolute state D p (c, r - 1)
  | e == R = let cps = setAbsolute c p 0 $ normalize (map fst $ cells state)
             in state { cells = zip cps $ map snd $ cells state }
  | e == D = state { cells = toNth (second $ (setAbsolute r p 0 . normalize))
                             c (cells state) }
  | otherwise = state

resizeCell state tx ty (ci, ri) = state
  { cells = toNth (second (toNth ty ri) . first tx) ci (cells state) }

toNth _ _ [] = []
toNth f 0 (x:xs) = (f x):xs
toNth f n (x:xs) = x:(toNth f (n-1) xs)

mouseResizeTile :: Rational -> (Window -> X ())  -> Window -> X ()
mouseResizeTile border fallback w =
  whenX (isClient w) $
  withDisplay $ \dpy -> do
  (Rectangle sx sy sw sh) <- gets (screenRect .
                                    W.screenDetail .
                                    W.current .
                                    windowset)
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
            (False, mouse, \px -> return ())

      (hitX, warpx, dragX) = drag ox wx ww L R
      (hitY, warpy, dragY) = drag oy wy wh U D

      dragHandler x y = do
        let xp = (fromIntegral x - fromIntegral sx) / fromIntegral sw
            yp = (fromIntegral y - fromIntegral sy) / fromIntegral sh
        dragX xp
        dragY yp
      stopHandler = return ()
  if hitX || hitY
    -- warp pointer here
    then do io $ warpPointer dpy none w 0 0 0 0 (floor warpx) (floor warpy)
            mouseDrag dragHandler stopHandler
    else fallback w
