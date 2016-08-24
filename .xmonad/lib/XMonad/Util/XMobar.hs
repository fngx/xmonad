{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module XMonad.Util.XMobar where

import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import Data.Char
import DBus
import DBus.Client
import Codec.Binary.UTF8.String ( decodeString )
import Graphics.UI.Gtk hiding ( Signal )
import qualified XMonad.Util.Colours as Cs

runWithBar cfg =
  do session <- connectSession
     let sendBus s = emit session (signal"/org/xmonad/Log" "org.xmonad.Log" "Update")
            {signalBody = [toVariant $ decodeString s]}
         pp = myPP { ppOutput = sendBus }
         log = dynamicLogWithPP pp
     xmonad $ cfg { logHook = (logHook cfg) >> log }

raw = escapeMarkup

taffyBold = wrap "<b>" "</b>"

taffyUl = wrap "<u>" "</u>"

taffyColor fg bg = wrap t "</span>"
  where
    t = concat ["<span fgcolor=\"", fg, if null bg then "" else "\" bgcolor=\"" ++ bg , "\">"]

myPP = xmobarPP
  { ppCurrent = taffyColor "white" "" . taffyBold . taffyUl
  , ppVisible = taffyColor "white" ""
  , ppHidden = \s -> case s of
      "▼" -> taffyColor "grey" "" s
      _ -> taffyColor "yellow" "" s
  , ppUrgent = taffyColor Cs.urgent ""
  , ppTitle = taffyColor "white" "" . raw . shorten 120
  , ppLayout = \s -> taffyBold $
                     case words s of
                       ["Full"] -> "+"
                       [l, "by", x] -> l ++ case x of
                                              "C" -> ""
                                              "F" -> "+"
                                              _ -> x
                       _ -> s
  , ppSep = taffyColor "#777777" "" " | "
  }
