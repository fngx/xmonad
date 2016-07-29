{-# LANGUAGE FlexibleContexts #-}
import XMonad hiding (config)

import qualified XMonad
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import qualified Data.Map as M
import XMonad.Util.EZConfig (additionalKeysP)
import System.Exit (exitWith, ExitCode (ExitSuccess))
import XMonad.Layout.NoBorders (smartBorders)
import qualified XMonad.Layout.Rows as R
import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks ( ToggleStruts (ToggleStruts) )
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Prompt.MyPrompt
import XMonad.Layout.Groups.Helpers as G
import qualified XMonad.Actions.Ring as Ring
import XMonad.Actions.Iconify (greedyFocusWindow, focusWindow, iconify, uniconify)
import Control.Monad (liftM2)
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import Data.Char (toLower)
import qualified XMonad.Actions.Warp as Warp
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Util.EMenu (hintSubmap)

main = xmonad config

followFocus = Warp.warpToWindow 0.9 0.1

wsLabels = ["q", "w", "e", "r", "t"]
icon = "▼"

resetLayout = do
  layout <- asks (layoutHook . XMonad.config)
  setLayout layout

layout c = c
  { layoutHook = l }
  where l = desktopLayoutModifiers $ smartBorders $ mkToggle (single NBFULL) $ R.rows

hooks c =
  withUrgencyHookC
  BorderUrgencyHook {urgencyBorderColor = "red"}
  urgencyConfig {suppressWhen = Never}
  $
  c
  { handleEventHook = (handleEventHook c) <+> fullscreenEventHook
  , manageHook = (manageHook c) <+>
                 composeAll
                 [ isDialog --> doFloat,
                   isFullscreen --> doFullFloat
                 ]
  , logHook = (logHook c) >> (Ring.update $ fmap (liftM2 (,) W.peek W.allWindows) (gets windowset))
  , startupHook = (startupHook c)
  }

config =
  pagerHints $
  hooks $
  layout $
  flip additionalKeysP bindings $
  desktopConfig
  { modMask = mod4Mask
  , workspaces = wsLabels ++ [icon]
  , keys = const $ M.empty
  , normalBorderColor  = "#333333"
  , focusedBorderColor = "dark cyan"
  , borderWidth = 1
}

bindings =
  [ -- xmonad controls
    ("M-S-<Escape>", io (exitWith ExitSuccess))
  , ("M-<Escape>", spawn "xmonad --recompile; xmonad --restart")

  -- keys to launch programs
  , ("M-S-<Return>", spawn "xterm")

  , ("M-a",
     hintSubmap config
     [ ("e", "emacs", spawn "emacsclient -c -n")
     , ("q", "qutebrowser", spawn "qb")
     , ("s h", "hibernate", spawn "systemctl hibernate")
     , ("s s", "suspend", spawn "systemctl suspend")
     ]
    )

  -- , ("M-a e", spawn "emacsclient -c -n")
  -- , ("M-a q", spawn "qb")

  -- , ("M-a s h", spawn "systemctl hibernate")
  -- , ("M-a s s", spawn "systemctl suspend")

  , ("M-x", shell)

  -- keys to adjust the stack and focus
  , ("M-k", kill)

  -- keys to adjust the layout
  , ("M-z", withFocused $ windows . W.sink)

  , ("M-p",   R.focusPrev)
  , ("M-n",   R.focusNext)
  , ("M-S-p", R.swapPrev)
  , ("M-S-n", R.swapNext)
  , ("M-;",   R.maximize)
  , ("M-S-;", R.equalize)

  , ("M-l M-l", R.groupNextLayout)
  , ("M-l r", resetLayout)

  , ("M-o", R.makeGroup)
  , ("M-<Return>", G.swapGroupMaster)

  -- this goes to the outer multitoggle
  , ("M-f", sendMessage $ Toggle NBFULL)

  -- minify
  , ("M-m", iconify icon)
  , ("M-,", uniconify icon)

  -- cycle and unminify
  , ("M-<Space>", do us <- readUrgents
                     case us of
                       (h:_) -> windows $ greedyFocusWindow icon h
                       [] -> Ring.rotate [xK_Super_L] xK_space (windows . (greedyFocusWindow icon)))

    -- one day I shall replace dmenu perhaps

  , ("M-b", bringMenuArgs ["-i", "-l", "10", "-p", "bring"])
  , ("M-g", gotoMenuArgs  ["-i", "-l", "10", "-p", "goto"])

  , ("M-s", swapNextScreen)
  , ("M-S-s", shiftNextScreen)
  , ("M-M1-s", nextScreen)
  ]
  ++
  -- workspace switching keys
  [ (mod ++ key, action key) |
    key <- wsLabels,
    (mod, action) <- [ ("M-", windows . W.greedyView)
                     , ("M-S-", windows . W.shift)] ]
