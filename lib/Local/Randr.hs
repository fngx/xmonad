module Local.Randr (randrKeys) where

import XMonad
import Graphics.X11.Xrandr
import Data.Maybe

resetRandr :: X ()
resetRandr = do
  XConf { display = dpy, theRoot = root } <- ask

  resM <- io $ xrrGetScreenResources dpy root

  io $ whenJust resM $ \res -> do
    outsM <- mapM (xrrGetOutputInfo dpy res) (xrr_sr_outputs res)

    -- we have the outputs
    -- now we want to disable all but one
    -- which we want to be the screen.

    putStrLn $ show $ mapMaybe id $ outsM

    -- now what do we do to change the outputs?

randrKeys =
  [ ("M-d", resetRandr)

  ]
