{-# LANGUAGE FlexibleContexts #-}
module Local.Manage (addManageRules) where

import qualified Local.Theme

import Control.Monad

import XMonad
import XMonad.Util.Run
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W
import XMonad.Actions.CopyWindow (copyToAll)

setBorder c w = withDisplay $ \d -> io $ do
  g <- initColor d c
  whenJust g $ \g' -> setWindowBorder d w g'

addManageRules c = withUrgencyHookC LibNotifyUrgencyHook
                   urgencyConfig { suppressWhen = Focused
                                 , remindWhen = Every 120 }
                   $ c { manageHook = (manageHook c) <+> windowRules
                       , logHook = (logHook c) >>
                                   (withFocused $ \w -> do
                                       us <- fmap null readUrgents
                                       when (not us) $ setBorder Local.Theme.hasUrgentBorderColor w)
                       }

windowRules = composeAll
  [ isDialog --> doFloat
  , transience'
  , isFullscreen --> doFullFloat
  , className =? "conky" --> (doF copyToAll) <+> (doSideFloat CE) ]

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        whenX (fmap not $ runQuery (className =? "qutebrowser") w) $ do
          name <- getName w
          wset <- gets windowset
          let Just idx = W.findTag w wset
          when (not $ idx `elem` (map (W.tag . W.workspace) $ (W.current wset):(W.visible wset))) $
            safeSpawn "notify-send" [(show name) ++ " urgent on " ++ idx, "-a", "xmonad"]

        setBorder Local.Theme.urgentBorderColor w
