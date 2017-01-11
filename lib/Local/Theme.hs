module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    Local.Theme.urgentBorderColor, urgentText,
                    hasUrgentBorderColor,
                    normalBorderColor, normalText,
                    otherWindow, smallFont
                   ) where

import XMonad.Layout.Decoration

focusedBorderColor   = "white"
normalBorderColor    = "black"
otherWindow          = "grey50"
hasUrgentBorderColor = "darkred"
urgentBorderColor    = "red"

focusedText = "black"
normalText = "white"
urgentText = "white"

smallFont = "xft:Terminus-9" --"xft:Liberation Mono-8"

decorations = def
  { fontName = smallFont
  , decoHeight = 14

  , activeBorderColor = focusedBorderColor
  , activeTextColor = focusedText
  , activeColor = focusedBorderColor

  , inactiveBorderColor = otherWindow
  , inactiveTextColor = normalText
  , inactiveColor = normalBorderColor

  , XMonad.Layout.Decoration.urgentBorderColor = Local.Theme.urgentBorderColor
  , urgentTextColor = urgentText
  , urgentColor = Local.Theme.urgentBorderColor
  }
