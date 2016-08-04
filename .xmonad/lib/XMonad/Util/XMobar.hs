{-# LANGUAGE FlexibleContexts #-}

module XMonad.Util.XMobar where

import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import Data.Char

runWithBar cfg =
  do pipe <- spawnPipe "xmobar"
     let pp = (myPP (focusedBorderColor cfg)) { ppOutput = hPutStrLn pipe }
         log = dynamicLogWithPP pp
     xmonad $ cfg { logHook = (logHook cfg) >> log }

raw s = wrap ("<raw=" ++ (show $ length s)++":") ("/>") s

font n = wrap ("<fn=" ++ (show n) ++ ">") "</fn>"

myPP c = xmobarPP
  { ppCurrent = xmobarColor c "" . font 1 -- . (map toUpper)
  , ppVisible = xmobarColor "white" "" . font 1
  , ppUrgent = xmobarColor "red" ""
  , ppTitle = xmobarColor "white" "" . raw . shorten 120
  , ppLayout = \s -> xmobarColor "grey" "" $ case s of
      "Full" -> "+"
      x:" by H" -> x:""
      x:" by Full" -> x:"+"
      s -> s
  }
