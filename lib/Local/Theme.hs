module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    urgentBorderColor, urgentText,
                    normalBorderColor, normalText,
                    otherWindow, smallFont, bigFont
                   , overflowWindow
                   ) where

import XMonad.Util.Font ( Align (..) )
import XMonad (Window, broadcastMessage, io, runQuery, className)
import XMonad.Layout.Decoration
import Local.Windows (nextInHistory)
import Local.Colors
import XMonad.Hooks.UrgencyHook (readUrgents)
import qualified XMonad.Actions.TagWindows as T

decorations = def
  { fontName       = smallFont
  , decoHeight     = 15

  , activeColors   = cs focusedBorderColor focusedBorderColor focusedText
  , inactiveColors = cs inactiveTabColor   "black" normalText
  , urgentColors   = cs urgentBorderColor  urgentBorderColor urgentText
  , perWindowTheme = \w -> do nextM <- nextInHistory
                              isOverflow <- T.hasTag "overflow" w
                              isUrgent <- fmap (elem w) readUrgents
                              className <- runQuery className w
                              let isNext = Just w == nextM
                                  cstyle
                                    | isOverflow = Just $ cs overflowWindow overflowWindow normalText
                                    | otherwise = Nothing
                                  rightText = concat [
                                    " (",
                                    if isNext then "• " else "",
                                    if isUrgent then "★ " else "",
                                    className,
                                    ") "
                                    ]
                              return $ (cstyle, [(rightText, AlignRight)])
  }
  where cs a b c = Colors { bgColor = a, borderColor = b, textColor = c }
