import XMonad hiding ( (|||) )
import Local.Hints
import Local.Layout
import Local.Manage
import Local.Prompts
import Local.Randr
--import Local.SloppyFocus
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
import XMonad.Util.Paste (pasteSelection)
import Local.MC (mouseResizeTile)

main = xmonad =<< xmobar conf

conf =
  Windows.addHistory $
--  sloppyFocus $
  addManageRules $
  addLayout $
  randrAuto $
  desktopConfig
  { modMask = mod4Mask
  , terminal = "urxvt"
  , borderWidth = 1
  , focusedBorderColor = Theme.focusedBorderColor
  , normalBorderColor = Theme.normalBorderColor
  , workspaces = fixedWorkspaces
  , focusFollowsMouse = True
  }
  `additionalMouseBindings`
  [((mod4Mask, 3), \w -> focus w >> (Flex.mouseResizeWindow w))
  , ((mod4Mask .|. shiftMask, 3), (\w -> focus w >> CR.mouseResizeWindow w True ))
  , ((mod4Mask, 2), \w -> focus w >> pasteSelection)
  , ((mod4Mask, 1), mouseResizeTile 150 mouseMoveWindow) -- todo always mousemove floating windows
--  , ((mod4Mask, 4), return ()) -- scroll up
    --  , ((mod4Mask, 5), return ()) -- scroll down
    --  , ((mod4Mask, 6), return ()) -- scroll left
    --  , ((mod4Mask, 7), return ()) -- scroll right
  ]
  `hintedKeysP`
  keys
  where keys = concat [layoutKeys,
                       spawnKeys,
                       promptKeys,
                       workspaceKeys,
                       randrKeys,
                       Windows.windowKeys]
