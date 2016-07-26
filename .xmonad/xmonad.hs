import XMonad hiding (config)
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

main = xmonad config

wsLabels = ["q", "w", "e", "r", "t"]
icon = "*"

layout c = c
  { layoutHook = l }
  where l = desktopLayoutModifiers $ smartBorders $ mkToggle (single NBFULL) $ R.rows

hooks c = c
  { handleEventHook = (handleEventHook c) <+> fullscreenEventHook
  , manageHook = (manageHook c) <+>
                 composeAll
                 [ isDialog --> doFloat,
                   isFullscreen --> doFullFloat ] }

config =
  pagerHints $
  flip additionalKeysP bindings $
  hooks $
  layout $
  desktopConfig
  { modMask = mod4Mask
  , workspaces = wsLabels ++ [icon]
  , keys = const $ M.empty }

bindings =
  [ -- xmonad controls
    ("M-S-<Escape>", io (exitWith ExitSuccess))
  , ("M-<Escape>", spawn "xmonad --recompile; xmonad --restart")

  -- keys to launch programs
  , ("M-S-<Return>", spawn "xterm")
  , ("M-a", spawn "dmenu_run")

  -- keys to adjust the stack and focus
  , ("M-<Backspace>", kill)

  -- keys to adjust the layout
  , ("M-z", withFocused $ windows . W.sink)

  , ("M-p", R.focusPrev)
  , ("M-n", R.focusNext)
  , ("M-l t", R.groupToTabbed)
  , ("M-l M-l", R.groupNextLayout)
  , ("M-f", sendMessage $ Toggle NBFULL)

  , ("M-;", R.makeGroup)
  , ("M-<Space>", sendMessage NextLayout)
  ]
  ++
  -- workspace switching keys
  [ (mod ++ key, action key) |
    key <- wsLabels,
    (mod, action) <- [ ("M-", windows . W.greedyView)
                     , ("M-S-", windows . W.shift)] ]
