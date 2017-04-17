module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    urgentBorderColor, urgentText,
                    normalBorderColor, normalText,
                    otherWindow, smallFont, bigFont
                   , overflowWindow
                   ) where

import XMonad (Window, broadcastMessage, io)
import XMonad.Layout.Decoration
import Local.Windows (nextInHistory)
import Local.Colors
import qualified XMonad.Actions.TagWindows as T

decorations = def
  { fontName       = smallFont
  , decoHeight     = 15

  , activeColors   = cs focusedBorderColor focusedBorderColor focusedText
  , inactiveColors = cs inactiveTabColor inactiveTabColor normalText
  , urgentColors   = cs urgentBorderColor urgentBorderColor urgentText
  , perWindowTheme = \w -> do nextM <- nextInHistory
                              isOverflow <- T.hasTag "overflow" w

                              let isNext = Just w == nextM
                                  style w
                                    | isOverflow= Just $ cs overflowWindow overflowWindow normalText
                                    | isNext = Just $ cs inactiveTabColor otherWindow normalText
                                    | otherwise = Nothing
                              return $ style w
  }
  where cs a b c = Colors { bgColor = a, borderColor = b, textColor = c }
