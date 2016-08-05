module XMonad.Util.HintedSubmap where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils
import XMonad.Util.Font
import Control.Monad
import XMonad.Util.EZConfig
import XMonad.Actions.Submap
import qualified Data.Map as M
import qualified Debug.Trace as D

data Theme = Theme
  { font :: String
  , bg :: String
  , border :: (Int, String)
  , sep :: String
  , key_c :: String
  , desc_c :: String
  , sep_c :: String
  , top :: Bool
  }

defaultTheme = Theme
  { font = "xft:Monospace-10"
  , bg = "#333333"
  , border = (1, "#888888")
  , sep = " → "
  , sep_c = "#999999"
  , key_c = "cyan"
  , desc_c = "white"
  , top = False
  }

drawHints :: Theme -> [(String, String)] -> X (Bool -> X ())
drawHints theme stuff = do
  XConf { display = d, theRoot = rw } <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset
  xmf <- initXMF (font theme)
  widths <- mapM (\(k, t) -> do a <- textWidthXMF d xmf k
                                b <- textWidthXMF d xmf t
                                return (a, b)) stuff

  sepw <- textWidthXMF d xmf (sep theme)

  (asc, dsc) <- textExtentsXMF xmf $ fst $ head stuff

  let colspc = 20
      rowspc = 3
      (bwidth, bcol) = border theme
      widestk = (maximum $ map fst widths)
      widestd = (maximum $ map snd widths)
--      widest =  maximum $ map (uncurry (+)) widths
      widest' = widestk + widestd + sepw + colspc
      cols = (fi sw) `div` widest'
      rows = ceiling $ (fi (length widths)) / (fi cols)
      th = asc + dsc
      wh :: Dimension
      wh = fi $ 2 * bwidth +
           (fi (rows * (fi th))) +
           (fi $ (1 + rows) * rowspc)
      wy = if (top theme) then 0 else (sy + (fi sh) - (fi wh))

  win <- createNewWindow (Rectangle sx wy sw wh)
         Nothing "Hints" True

  showWindow win
  paintWindow win sw wh (fi bwidth) (bg theme) bcol

  pxm <- io $ createPixmap d win sw wh $
         (defaultDepthOfScreen $ defaultScreenOfDisplay d)

  gc <- io $ createGC d pxm

  io $ setGraphicsExposures d gc False

  let x0 = bwidth + 8
      y0 = asc + (fi rowspc)
  -- paint columns
  foldM_
    (\(x, y, z) ((key, lbl), (kw, lw)) -> do
        -- TODO align on arrow
        printStringXMF d pxm xmf gc (key_c theme) (bg theme) (x + (fi (widestk - kw))) y key
        printStringXMF d pxm xmf gc (sep_c theme) (bg theme) (x + (fi widestk)) y (sep theme)
        printStringXMF d pxm xmf gc (desc_c theme) (bg theme) (x+(fi widestk)+(fi sepw)) y lbl
        let delta = if (rows == 1) then (fi widestk) + (fi sepw) + (fi colspc) + (fi lw)
                    else (fi widest')
        return $ if (rows > 1) && z == (cols - 1) then
                   (fi x0, y + asc + dsc + (fi rowspc), 0)
                 else (x + delta, y, z + 1))
    (fi x0, fi y0, 0) (zip stuff widths)

  io $ copyArea d pxm win gc 0 0 sw wh 0 0

  -- window is drawn, return the release handler

  return $ (\x -> do deleteWindow win
                     when x $ do io $ freeGC d gc
                                 io $ freePixmap d pxm
                                 releaseXMF xmf)


  -- drawColumn dsp pxm gc fg bg aln x0 x1 text =
  -- foldM_ drawSkip 0 text
  -- where drawSkip y0 str = do
  --         (x, y) <- stringPosition dsp fnt (Rectangle x0 x1  d) aln str
  --         printStringXMF dsp pxm fnt gc fg bg x y str
  --         return y


hintSubmap :: XConfig l -> [(String, String, X ())] -> X ()
hintSubmap c keys = do
  let dm = map (\(x, y, _) -> (x, y)) keys
  cleanup <- drawHints (defaultTheme {key_c = (focusedBorderColor c)}) dm
  let km = mkKeymap c $ fmap (\(x, y, z) -> (x, cleanup False >> z)) keys
  submap km
  cleanup True
