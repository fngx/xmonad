module Local.Theme (decorations,
                    focusedBorderColor, focusedText,
                    urgentBorderColor, urgentText,
                    normalBorderColor, normalText,
                    singleSelectionColor, multiSelectionColor,
                    otherWindow, smallFont, bigFont
                   , overflowWindow, singleSelectionStyle, multiSelectionStyle
                   , resetStyles, styleWindows, urgentStyle, nextStyle, overflowStyle
                   ) where

import XMonad.Util.Font ( Align (..) )
import XMonad (Window, broadcastMessage, io, runQuery, className, X)
import XMonad.Layout.Decoration
import Local.Windows (nextInHistory)
import Local.Colors
import XMonad.Hooks.UrgencyHook (readUrgents)
import qualified XMonad.Actions.TagWindows as T

styleWindows :: [Window] -> WindowStyle -> X ()
styleWindows ws s = mapM_ (flip styleWindow s) ws

urgentStyle   = WindowStyle Nothing [("! ", AlignRight)]
nextStyle     = WindowStyle Nothing [("★ ", AlignRight)]
overflowStyle = WindowStyle (Just cs)  []
  where cs = Colors { bgColor = overflowWindow, borderColor = overflowWindow, textColor = normalText }

singleSelectionStyle = WindowStyle (Just cs)  []
  where cs = Colors { bgColor = singleSelectionColor, borderColor = singleSelectionColor, textColor = "black" }

multiSelectionStyle = WindowStyle (Just cs)  []
  where cs = Colors { bgColor = multiSelectionColor, borderColor = multiSelectionColor, textColor = "black" }


decorations = def
  { fontName       = smallFont
  , decoHeight     = 15

  , activeColors   = cs focusedBorderColor focusedBorderColor focusedText
  , inactiveColors = cs inactiveTabColor  "lightsteelblue4" normalText
  , urgentColors   = cs urgentBorderColor  urgentBorderColor urgentText
  }
  where cs a b c = Colors { bgColor = a, borderColor = b, textColor = c }
