module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    Local.Theme.urgentBorderColor, urgentText,
                    hasUrgentBorderColor,
                    normalBorderColor, normalText,
                    singletonBorderColor,
                    otherWindow
                   ) where

import XMonad.Layout.Decoration

focusedBorderColor   = "orange"
singletonBorderColor = "white"
normalBorderColor    = "grey30"
otherWindow          = "deepskyblue"
hasUrgentBorderColor = "magenta1"
urgentBorderColor    = "darkred"

focusedText = "black"
normalText = "white"
urgentText = "white"

decorations = def
  { fontName = "xft:Liberation Mono-8"
  , decoHeight = 14

  , activeBorderColor = focusedBorderColor
  , activeTextColor = focusedText
  , activeColor = focusedBorderColor

  , inactiveBorderColor = "#aaa"
  , inactiveTextColor = normalText
  , inactiveColor = normalBorderColor

  , XMonad.Layout.Decoration.urgentBorderColor = Local.Theme.urgentBorderColor
  , urgentTextColor = urgentText
  , urgentColor = Local.Theme.urgentBorderColor
  }
