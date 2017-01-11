import XMonad hiding ( (|||) )
import Local.Hints
import Local.Layout
import Local.Manage
import Local.Prompts
import Local.Randr
import Local.SloppyFocus
import Local.Spawn
import Local.Workspaces
import Local.XMobar (xmobar)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.EZConfig
import qualified Data.Map.Strict as M (empty)
import qualified Local.Theme as Theme
import qualified Local.Windows as Windows
import qualified XMonad.Actions.ConstrainedResize as CR
import qualified XMonad.Actions.FlexibleResize as Flex

main = xmonad =<< xmobar conf

conf =
  Windows.addHistory $
  sloppyFocus $
  addManageRules $
  addLayout $
  desktopConfig
  { modMask = mod4Mask
  , terminal = "urxvt"
  , borderWidth = 1
  , focusedBorderColor = Theme.focusedBorderColor
  , normalBorderColor = Theme.normalBorderColor
  , workspaces = fixedWorkspaces
  , focusFollowsMouse = False
  }
  `additionalMouseBindings`
  [((mod4Mask, 3), \w -> focus w >> (Flex.mouseResizeWindow w)),
   ((mod4Mask, 2), (\w -> focus w >> CR.mouseResizeWindow w True ))]
  `hintedKeysP`
  keys
  where keys = concat [layoutKeys,
                       spawnKeys,
                       promptKeys,
                       workspaceKeys,
                       randrKeys,
                       Windows.windowKeys]
