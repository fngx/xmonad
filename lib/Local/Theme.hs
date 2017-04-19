module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    urgentBorderColor, urgentText,
                    normalBorderColor, normalText,
                    otherWindow, smallFont, bigFont
                   , overflowWindow
                   ) where

import XMonad.Util.Font ( Align (..) )
import XMonad (Window, broadcastMessage, io)
import XMonad.Layout.Decoration
import Local.Windows (nextInHistory)
import Local.Colors
import XMonad.Hooks.UrgencyHook (readUrgents)
import qualified XMonad.Actions.TagWindows as T

decorations = def
  { fontName       = smallFont
  , decoHeight     = 15

  , activeColors   = cs focusedBorderColor focusedBorderColor focusedText
  , inactiveColors = cs inactiveTabColor   "grey30" normalText
  , urgentColors   = cs urgentBorderColor  urgentBorderColor urgentText
  , perWindowTheme = \w -> do nextM <- nextInHistory
                              isOverflow <- T.hasTag "overflow" w
                              isUrgent <- fmap (elem w) readUrgents
                              let isNext = Just w == nextM
                                  cstyle
                                    | isOverflow = Just $ cs overflowWindow overflowWindow normalText
                                    | otherwise = Nothing
                                  astyle
                                    | isNext = [("• ", AlignRight)]
                                    | otherwise = []
                                  ustyle
                                    | isUrgent = [("★ ", AlignRight)]
                                    | otherwise = []
                              return $ (cstyle, astyle ++ ustyle)
  }
  where cs a b c = Colors { bgColor = a, borderColor = b, textColor = c }
