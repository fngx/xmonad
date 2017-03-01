{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, Rank2Types #-}

module Local.Layout (addLayout, layoutKeys) where

import Local.Windows (recentWindows)
import Local.Workspaces (warp)
import qualified Local.Theme as Theme

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Layout.Tabbed
import XMonad.StackSet (Workspace (Workspace), Stack (..))
import XMonad.Layout.TrackFloating
import XMonad.Layout.NoBorders
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Hooks.ManageDocks (ToggleStruts (ToggleStruts), SetStruts (SetStruts))
import XMonad.Layout.LayoutCombinators
import XMonad.Actions.RotSlaves
import Local.MC

tabs = tabbed shrinkText Theme.decorations

layout = trackFloating $
         lessBorders OnlyFloat $
         mkToggle (single FULL) $
         twocol
         -- TODO: fix ifWider to work here (it doesn't Hide, so dangling tabs)
  where
    twocol = mc tabs [(1, [1]), (1, [1])]
    onecol = mc tabs [(1, [1])]

addLayout c =
  c { layoutHook = layout }

layoutKeys =
  let col n = (1, take n $ repeat 1)
      sendMC :: MCMsg Window -> X ()
      sendMC = sendMessage

      equalize _ a = zip (repeat 1) $ map ((map (const 1)) . snd) a
      delRow Nothing as = as
      delRow (Just (c, r)) as = let (al, ar) = splitAt c as
                                    (cw, rs) = head ar
                                    (rsu, rsd) = splitAt r rs
                                    rs' = rsu++(tail rsd)
                                in if null rs'
                                   then let dc = al++(tail ar) in
                                          if null dc then as
                                          else dc
                                   else al++((cw,rs'):(tail ar))
      addRow Nothing as = as
      addRow (Just (c, r)) as = let (al, ar) = splitAt c as
                                    (cw, rs) = head ar
                                in al ++ ((cw, 1:rs):(tail ar))

      addCol Nothing as = as
      addCol (Just (c, r)) as = let (al, ar) = splitAt c as
                                in al++((1, [1]):ar)

      delCol Nothing as = as
      delCol (Just (c, r)) as = if length as == 1 then as
                                else let (al, ar) = splitAt c as
                                     in al++(tail ar)

  in
  [ ("M-n", ("down", windows W.focusDown >> warp))
  , ("M-p", ("up",   windows W.focusUp >> warp))

  , ("M-m",   ("focus master",  windows W.focusMaster >> warp))
  , ("M-S-m", ("shift master", windows W.shiftMaster >> warp))
  , ("M-z",   ("swap master", windows W.swapMaster >> warp))
  , ("M-j",   ("focus 2nd", windows (W.focusDown . W.focusMaster) >> warp))
  , ("M-S-j", ("shift 2nd", windows (W.swapDown . W.shiftMaster) >> warp))

  , ("M-l 1",   ("full",   sendMC $ SetCells [col 1] ))
  , ("M-l 2",   ("1|1", sendMC $ SetCells [col 1, col 1] ))
  , ("M-l 3",   ("1|2", sendMC $ SetCells [col 1, col 2] ))
  , ("M-l 4",   ("1|3", sendMC $ SetCells [col 1, col 3] ))
  , ("M-l e",   ("equalize", withFocused $ (sendMC . ChangeCells equalize)))

  , ("M-=", ("grow", withFocused $ (sendMC . (ResizeCell 0.2 0.2))))
  , ("M--", ("shrink", withFocused $ (sendMC . (ResizeCell (-0.2) (-0.2)))))

  , ("M-,", ("- row", withFocused $ sendMC . (ChangeCells delRow)))
  , ("M-.", ("+ row", withFocused $ sendMC . (ChangeCells addRow)))

  , ("M-S-,", ("- row", withFocused $ sendMC . (ChangeCells delCol)))
  , ("M-S-.", ("+ row", withFocused $ sendMC . (ChangeCells addCol)))

  , ("M-M1-n", ("rfd", rotSlavesDown))
  , ("M-M1-p", ("rfd", rotSlavesUp))

  , ("M-S-n", ("swap down", windows W.swapDown >> warp))
  , ("M-S-p", ("swap up", windows W.swapUp >> warp))

  , ("M-f", ("full", sendMessage $ Toggle FULL))
  , ("M-v", ("flip", sendMC Flip))

  , ("M-S-b", ("no dock", (broadcastMessage $ SetStruts [] [minBound .. maxBound]) >>
                          spawn "pkill -STOP xmobar" >> refresh))
  , ("M-b",   ("all dock", (broadcastMessage $ SetStruts [minBound .. maxBound] []) >>
                           spawn "pkill -CONT xmobar" >> refresh))

  , ("M-k", ("kill", kill))]
