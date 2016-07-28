module XMonad.Util.EMenu where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils
import XMonad.Util.Font
import Control.Monad

data Style = Style

emenu :: Style -> (String -> IO [a]) -> (a -> String) -> X (Maybe a)
emenu style generate render = do
  XConf { display = d, theRoot = rw } <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  font <- initXMF "xft:Monospace-12"
  -- determine line height of font and use it to work out window height
  (ascent, descent) <- textExtentsXMF font "a"
  let wh = fi $ (ascent + descent) * 10

  win <- createNewWindow (Rectangle sx sy sw wh) Nothing "What" True
  showWindow win

  -- this draws a box on the window, but really we want to draw a few things
  -- namely an input area and selection/results area, maybe having a description column or something
  paintAndWrite win
    font sw wh 2 "#333333" "dark cyan"
    "white" "#333333" [AlignLeft] ["lorem ipsem dolor sit"]
  status <- io $ grabKeyboard d win True grabModeAsync grabModeAsync currentTime
  when (status == grabSuccess) $ do
    io $ allocaXEvent $ \e -> do
      maskEvent d keyPressMask e
      ev <- getEvent e -- block for a keypress;; here we would run our loop
      return ()
    io $ ungrabKeyboard d currentTime
  deleteWindow win
  releaseXMF font
  io $ sync d False
  return $ Nothing
  -- create window
  -- generate inputs
  -- render inputs
  -- read keys and regenerate input
  -- todo: thread results through in some way for state
