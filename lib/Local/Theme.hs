module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    Local.Theme.urgentBorderColor, urgentText,
                    hasUrgentBorderColor,
                    normalBorderColor, normalText,
                    otherWindow, smallFont, bigFont
                   ,prevWindow
                   ) where

import XMonad.Layout.Decoration

focusedBorderColor   = "white"
normalBorderColor    = "grey20"
otherWindow          = "darkcyan"
prevWindow           = "slateblue3"
hasUrgentBorderColor = "darkred"
urgentBorderColor    = "red"

focusedText = "black"
normalText = "white"
urgentText = "white"

smallFont = "xft:Sans-8" --"xft:Liberation Mono-8"
bigFont = "xft:Sans-12"

decorations = def
  { fontName = smallFont
  , decoHeight = 15

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
