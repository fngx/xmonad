{-# LANGUAGE FlexibleContexts #-}

module Local.XMobar (Local.XMobar.xmobar) where

import Local.Workspaces (nonEmptyNames)

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Hooks.DynamicLog

import XMonad.Util.Run (spawnPipe, hPutStrLn)

import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)

xmobar' :: LayoutClass l Window
        => String -> XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar' cmd conf = do
  h <- spawnPipe cmd
  return $ docks $ conf
    { layoutHook = avoidStruts (layoutHook conf)
    , logHook = do logHook conf
                   names <- nonEmptyNames
                   dynamicLogWithPP $ clickNames names $ pp { ppOutput = hPutStrLn h }
    }

xmobar :: LayoutClass l Window
        => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = xmobar' "xmobar ~/.xmonad/xmobarrc"

pp = xmobarPP
  { ppCurrent = xmobarColor "deepskyblue" "" . esc
  , ppVisible = xmobarColor "white" "" . esc
  , ppHidden  = xmobarColor "grey50" "" . esc
  , ppLayout = const ""
  , ppTitle = xmobarColor "white" "" . esc
  }

esc = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

clickNames names pp =
  pp { ppCurrent = click (ppCurrent pp)
     , ppVisible = click (ppVisible pp)
     , ppHidden  = click (ppHidden pp)
     }
  where click :: (String -> String) -> String -> String
        click f s = concat ["<action=xdotool key super+",
                            (show $ 1+(fromJust$elemIndex s names)),
                            ">",
                            f s,
                            "</action>"]
