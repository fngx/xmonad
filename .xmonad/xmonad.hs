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
import XMonad.Actions.WindowBringer
import XMonad.Util.EMenu (hintSubmap)
import XMonad.Util.TemporaryBar

main = xmonad config

popBar = tempShowBar 0.75

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
  { handleEventHook = (handleEventHook c) <+> fullscreenEventHook <+>
    toggleBarHook
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
  flip additionalKeysP (map (\(k, _, a) -> ("M-" ++ k, a)) bindings) $
  desktopConfig
  { modMask = mod4Mask
  , workspaces = wsLabels ++ [icon]
  , keys = const $ M.empty
  , normalBorderColor  = "#888888"
  , focusedBorderColor = "cyan"
  , borderWidth = 1
}

bindings =
  [ -- xmonad controls
    ("S-<Escape>", "exit", io (exitWith ExitSuccess))
  , ("<Escape>", "restart", spawn "xmonad --recompile; xmonad --restart")

  -- keys to launch programs
  , ("S-<Return>", "terminal", spawn "xterm")

  , ("a", "run keys",
     hintSubmap config
     [ ("e", "emacs", spawn "emacsclient -c -n")
     , ("q", "qutebrowser", spawn "qb")
     , ("r", "prompt", shell)
     , ("c", "chromium", spawn "chromium")
     , ("p", "passwords", spawn "nop")
     , ("t", "htop", spawn "xterm -e htop")
     , ("h", "hibernate", spawn "systemctl hibernate")
     , ("s", "suspend", spawn "systemctl suspend")
     , ("m", "check mail", spawn "notmuch new")
     ]
    )

  -- keys to adjust the stack and focus
  , ("k", "kill window", kill)

  -- keys to adjust the layout
  , ("z", "sink window", withFocused $ windows . W.sink)

  , ("p",   "focus up", R.focusPrev)
  , ("n",   "focus down", R.focusNext)
  , ("S-p", "swap up", R.swapPrev)
  , ("S-n", "swap down", R.swapNext)
  , (";",   "max col", R.maximize)
  , ("S-;", "eq col", R.equalize)

  , ("l M-l", "group layout", R.groupNextLayout)
  , ("l r", "reset layout", resetLayout)

  , ("o", "+ column", R.makeGroup)
  , ("<Return>","swap master", G.swapGroupMaster)

  -- this goes to the outer multitoggle
  , ("f", "fullscreen", popBar >> (sendMessage $ Toggle NBFULL))

  -- minify
  , ("m", "minify", popBar >> iconify icon)
  , (",", "unminify", popBar >> uniconify icon)

  -- cycle and unminify
  , ("<Space>", "cycle focus",
     do us <- readUrgents
        case us of
          (h:_) -> windows $ greedyFocusWindow icon h
          [] -> Ring.rotate [xK_Super_L] xK_space (windows . (greedyFocusWindow icon))
        resetBar)

    -- one day I shall replace dmenu perhaps

  , ("b", "bring window", bringMenuArgs ["-i", "-l", "10", "-p", "bring"])
  , ("g", "find window", gotoMenuArgs  ["-i", "-l", "10", "-p", "goto"])
  , ("c", "toggle bar", toggleBar) -- nop nop

  , ("s", "swap screen", popBar >> swapNextScreen)
  , ("S-s", "shift screen", popBar >> shiftNextScreen)
  , ("M1-s", "focus screen", popBar >> nextScreen)

  , ("S-/", "this page", hintSubmap config bindings)
  ]
  ++
  -- workspace switching keys
  [ (mod ++ key, dsc ++ key, action key) |
    key <- wsLabels,
    (mod, dsc, action) <- [ ("", "view ", \t -> (windows $ W.greedyView t) >> popBar)
                          , ("S-", "shift ", \t -> (windows $ W.shift t) >> popBar)] ]
