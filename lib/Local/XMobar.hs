{-# LANGUAGE FlexibleContexts #-}

module Local.XMobar (Local.XMobar.xmobar) where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog

import XMonad.Util.Run (spawnPipe, hPutStrLn)

xmobar' :: LayoutClass l Window
        => String -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar' cmd conf = do
  h <- spawnPipe cmd
  return $ docks $ conf
    { layoutHook = avoidStruts (layoutHook conf)
    , logHook = do logHook conf
                   dynamicLogWithPP pp { ppOutput = hPutStrLn h }
    }

xmobar :: LayoutClass l Window
        => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = xmobar' "xmobar ~/.xmonad/xmobarrc"

pp = xmobarPP
  { ppCurrent = xmobarColor "deepskyblue" ""
  , ppVisible = xmobarColor "white" ""
  , ppHidden  = xmobarColor "grey50" ""
  , ppLayout = const ""
  , ppTitle = xmobarColor "white" ""
  }
