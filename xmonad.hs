import XMonad hiding ( (|||) )
import Local.Hints
import Local.Layout
import Local.Manage
import Local.Prompts
import Local.Randr
--import Local.SloppyFocus
import Local.Spawn
import Local.Workspaces
import Local.Util (isFloating)
import Local.XMobar (xmobar)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.EZConfig
import qualified Data.Map.Strict as M (empty)
import qualified Local.Theme as Theme
import qualified Local.Windows as Windows
import qualified XMonad.Actions.ConstrainedResize as CR
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Util.Paste (pasteSelection)
import Local.MC (mouseResizeTile, MCMsg(ResizeCell))
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.Layout.Fullscreen as FS

main = xmonad =<< xmobar conf

conf =
  FS.fullscreenSupport $
  Windows.addHistory $
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
  , ((mod4Mask, 1), \w -> do float <- isFloating w
                             if float
                               then (focus w >> Flex.mouseWindow Flex.discrete w)
                               else mouseResizeTile (1/3) mouseMoveWindow w)
  , ((mod4Mask, 4), sendMessage . ResizeCell (-0.1) 0) -- scroll up
  , ((mod4Mask, 5), sendMessage . ResizeCell 0.1 0) -- scroll down
  , ((mod4Mask, 6), sendMessage . ResizeCell 0 (-0.1)) -- scroll left
  , ((mod4Mask, 7), sendMessage . ResizeCell 0 0.1) -- scroll right
  , ((mod4Mask .|. shiftMask, 4), sendMessage . ResizeCell 0 (-0.1)) -- scroll left
  , ((mod4Mask .|. shiftMask, 5), sendMessage . ResizeCell 0 0.1) -- scroll right
  ]
  `hintedKeysP`
  keys
  where keys = concat [layoutKeys,
                       spawnKeys,
                       promptKeys,
                       workspaceKeys,
                       randrKeys,
                       Windows.windowKeys]
