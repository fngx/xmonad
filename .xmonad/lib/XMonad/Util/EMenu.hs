module XMonad.Util.EMenu where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils
import XMonad.Util.Font
import Control.Monad
import XMonad.Util.EZConfig
import XMonad.Actions.Submap
import qualified Data.Map as M
import qualified Debug.Trace as D

hintSubmap :: XConfig l -> [(String, String, X ())] -> X ()
hintSubmap c keys =
  let km = mkKeymap c $ fmap (\(x, y, z) -> (x, z)) keys
      binds = map (\(x,_,_) -> x) keys
      descrs = map (\(_,x,_) -> x) keys
  in
    do XConf { display = d, theRoot = rw } <- ask
       (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset
       font <- initXMF "xft:Monospace-12"
       let widths = mapM (textWidthXMF d font)
       dims <- liftM2 zip (widths binds) (widths descrs)
       (ascent, descent) <- textExtentsXMF font (head descrs)
       let maxDim =  maximum (map (\(a,b) -> (a+b)) dims)
           cols =  (fi sw) `div` (fi maxDim)
           rows =  max 1 $ (length dims) `div` (fi cols)
           wh =  fi $ 8+rows * (fi (ascent + descent))

       win <- createNewWindow (Rectangle sx (sy+(fi sh)-(fi wh)) sw wh) Nothing "Hints" True
       showWindow win
       paintWindow win sw wh 1 "#333333" "red"

       p <- io $ createPixmap d win sw wh $ (defaultDepthOfScreen $ defaultScreenOfDisplay d)
       gc <- io $ createGC d p

       io $ setGraphicsExposures d gc False

       arrw <- textWidthXMF d font " ->    "

       forM_ (zip3 (scanl (\x (a, b) -> a+b+x+arrw) 10 dims) binds descrs) $ \(w, bind, descr) ->
         printStringXMF d p font gc "white" "#333333" (fi w) (1+ascent) (bind ++ " -> " ++ descr)

       io $ copyArea      d p win gc 0 0 sw wh 0 0

       io $ freeGC d gc
       io $ freePixmap d p

       submap km
       deleteWindow win
       releaseXMF font

--
--   showWindow win

--   -- this draws a box on the window, but really we want to draw a few things
--   -- namely an input area and selection/results area, maybe having a description column or something
--   paintAndWrite win
--     font sw wh 2 "#333333" "dark cyan"
--     "white" "#333333" [AlignLeft] ["lorem ipsem dolor sit"]
--   status <- io $ grabKeyboard d win True grabModeAsync grabModeAsync currentTime
--   when (status == grabSuccess) $ do
--     io $ allocaXEvent $ \e -> do
--       maskEvent d keyPressMask e
--       ev <- getEvent e -- block for a keypress;; here we would run our loop
--       return ()
--     io $ ungrabKeyboard d currentTime
--   deleteWindow win
--   releaseXMF font
--   io $ sync d False
--   return $ Nothing
--   -- create window
--   -- generate inputs
--   -- render inputs
--   -- read keys and regenerate input
--   -- todo: thread results through in some way for state
