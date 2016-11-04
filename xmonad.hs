import XMonad hiding ( (|||) )

import Local.Layout
import Local.Spawn
import Local.Fullscreen
import Local.Prompts
import Local.Workspaces
import Local.Manage
import Local.Randr
import qualified Local.Windows as Windows

import qualified Local.Theme as Theme

import XMonad.Util.EZConfig
import XMonad.Hooks.EwmhDesktops (ewmh)
import qualified XMonad.Actions.FlexibleResize as Flex
import qualified XMonad.Actions.ConstrainedResize as CR

main = xmonad $
  ewmh $
  addManageRules $
  enableFullscreen
  conf

conf = def
  { modMask = mod4Mask
  , terminal = "urxvt"
  , layoutHook = layout
  , borderWidth = 2
  , focusedBorderColor = Theme.focusedBorderColor
  , normalBorderColor = Theme.normalBorderColor
  , workspaces = fixedWorkspaces
  , logHook = Windows.updateHistory
  }
  `additionalKeysP` keys
  `additionalMouseBindings`
  [((mod4Mask, 3), \w -> focus w >> (Flex.mouseResizeWindow w)),
   ((mod4Mask, 2), (\w -> focus w >> CR.mouseResizeWindow w True ))]
  where keys = concat [layoutKeys,
                       spawnKeys,
                       promptKeys,
                       workspaceKeys,
                       randrKeys,
                       Windows.windowKeys]
