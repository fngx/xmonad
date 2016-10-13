module Local.Theme (decorations,
                    focusedBorderColor,
                    Local.Theme.urgentBorderColor,
                    normalBorderColor) where

import XMonad.Layout.Decoration

focusedBorderColor = "red"
normalBorderColor = "#888888"

urgentBorderColor = "#ffffaa"

decorations = def
  { fontName = "xft:Fixed-10"
  , decoHeight = 14

  , activeBorderColor = "darkred"
  , activeTextColor = "white"
  , activeColor = "darkred"

  , inactiveBorderColor = "#666666"
  , inactiveTextColor = "white"
  , inactiveColor = normalBorderColor

  , XMonad.Layout.Decoration.urgentBorderColor = Local.Theme.urgentBorderColor
  , urgentTextColor = "black"
  , urgentColor = Local.Theme.urgentBorderColor
  }
