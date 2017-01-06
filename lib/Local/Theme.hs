module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    Local.Theme.urgentBorderColor, urgentText,
                    hasUrgentBorderColor,
                    secondaryColor, secondaryText,
                    normalBorderColor, normalText,
                    singletonBorderColor,
                    otherWindow
                   ) where

import XMonad.Layout.Decoration

singletonBorderColor = "lightgreen"

focusedBorderColor = "cyan"
focusedText = "black"

normalBorderColor = "#111"
normalText = "white"

otherWindow = "maroon1"

secondaryColor = "#4477bb"
secondaryText = "white"

urgentBorderColor = "darkred"
urgentText = "white"

hasUrgentBorderColor = "yellow"

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
