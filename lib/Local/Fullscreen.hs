module Local.Fullscreen (enableFullscreen) where

import XMonad
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

enableFullscreen c =
  c { handleEventHook = fullscreenEventHook <+> handleEventHook c
    , manageHook =
      composeAll [isFullscreen --> doFullFloat,
                  (manageHook c)]
    }
