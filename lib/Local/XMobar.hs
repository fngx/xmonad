{-# LANGUAGE FlexibleContexts #-}

module Local.XMobar (Local.XMobar.xmobar) where

import Local.Workspaces (nonEmptyNames)
import Local.Colors (focusedBorderColor, focusedText, urgentBorderColor, urgentText)

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
  return $
    --docks $
    conf
    { layoutHook = avoidStruts (layoutHook conf)
    , logHook = do logHook conf
                   names <- nonEmptyNames
                   dynamicLogWithPP $ clickNames names $ pp { ppOutput = hPutStrLn h }
    }

xmobar :: LayoutClass l Window
        => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = xmobar' "xmobar ~/.xmonad/xmobarrc"

pp = xmobarPP
  { ppCurrent = xmobarColor focusedText Local.Colors.focusedBorderColor . esc
  , ppVisible = xmobarColor "white" "grey30" . esc
  , ppHidden  = xmobarColor "grey80" "" . esc
  , ppLayout = esc
  , ppTitle = xmobarColor "white" "" . esc
  , ppUrgent = xmobarColor urgentText urgentBorderColor . esc
  }

esc = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

clickNames names pp =
  pp { ppCurrent = click (ppCurrent pp)
     , ppVisible = click (ppVisible pp)
     , ppHidden  = click (ppHidden pp)
     , ppUrgent  = click (ppUrgent pp)
     }
  where nameCount = length names
        click :: (String -> String) -> String -> String
        click f s =
          let ix = elemIndex s names
              wrapped n =
                let n' = (show $ 1+n)
                    s' = if s == "" then "∅" else s
                    s'' = if nameCount > 3 then n'++":"++s' else s'
                in
                  concat ["<action=xdotool key super+",
                           n',
                           ">",
                           f s'',
                           "</action>"]
          in
          case ix of
            Just n -> if n > 8 then f s
                      else wrapped n
            Nothing -> f s
