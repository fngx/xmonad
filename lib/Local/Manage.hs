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
import qualified Data.Map as M
import qualified Debug.Trace as D

import Local.Marks (allMarked)
import Local.Windows (recentWindows, nextInHistory)
import Data.Maybe (listToMaybe)

setBorder c w = withDisplay $ \d -> io $ do
  g <- initColor d c
  whenJust g $ \g' -> setWindowBorder d w g'

setBorderWidth b w = withDisplay $ \d -> io $ do setWindowBorderWidth d w b

setBorderHook =
    do vis <- gets (concat .
                    map (W.integrate' . W.stack . W.workspace) .
                    (\s -> (W.current s):(W.visible s)) .
                    windowset)

       mapM_ (setBorder Local.Theme.normalBorderColor) vis

       withFocused (setBorder Local.Theme.focusedBorderColor)

       nextM <- nextInHistory False
       prevM <- nextInHistory True

       whenJust nextM $ setBorder Local.Theme.otherWindow
       whenJust prevM $ setBorder Local.Theme.prevWindow

       us <- readUrgents
       mapM_ (setBorder Local.Theme.urgentBorderColor) us

       unless (null us) $ withFocused (setBorder Local.Theme.hasUrgentBorderColor)

addManageRules c = withUrgencyHookC LibNotifyUrgencyHook
                   urgencyConfig { suppressWhen = Focused
                                 , remindWhen = Every 120 }
                   $ c { manageHook = (manageHook c) <+> windowRules
                       , logHook = (logHook c) >> setBorderHook
                       }

windowRules = composeAll
  [ isDialog --> doFloat
  , transience'
  , className =? "Xmessage" --> doFloat
  , className =? "XClock" --> doFloat ]

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
