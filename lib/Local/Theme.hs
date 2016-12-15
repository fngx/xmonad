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

singletonBorderColor = "magenta"

focusedBorderColor = "orange1"
focusedText = "black"

normalBorderColor = "#111"
normalText = "white"

otherWindow = "darkcyan"

secondaryColor = "#4477bb"
secondaryText = "white"

urgentBorderColor = "red"
urgentText = "white"

hasUrgentBorderColor = "yellow"

decorations = def
  { fontName = "xft:Sans:pixelsize=10:bold"
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
