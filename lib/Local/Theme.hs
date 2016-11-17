module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    Local.Theme.urgentBorderColor, urgentText,
                    hasUrgentBorderColor,
                    secondaryColor, secondaryText,
                    normalBorderColor, normalText) where

import XMonad.Layout.Decoration

focusedBorderColor = "#00ff7f"
focusedText = "black"

normalBorderColor = "#000000"
normalText = "white"

secondaryColor = "#4477bb"
secondaryText = "white"

urgentBorderColor = "#ff0000"
urgentText = "white"

hasUrgentBorderColor = "#ff00ff"

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
