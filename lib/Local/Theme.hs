module Local.Theme (decorations,
                    focusedBorderColor,
                    Local.Theme.urgentBorderColor,
                    normalBorderColor) where

import XMonad.Layout.Decoration

focusedBorderColor = "red"
normalBorderColor = "#888888"

urgentBorderColor = "#ffffaa"

decorations = def
  { fontName = "xft:Fixed-8"
  , decoHeight = 12

  , activeBorderColor = focusedBorderColor
  , activeTextColor = "white"
  , activeColor = "red"

  , inactiveBorderColor = normalBorderColor
  , inactiveTextColor = "white"
  , inactiveColor = normalBorderColor

  , XMonad.Layout.Decoration.urgentBorderColor = Local.Theme.urgentBorderColor
  , urgentTextColor = "black"
  , urgentColor = Local.Theme.urgentBorderColor
  }
