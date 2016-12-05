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

import qualified Debug.Trace as D

setBorder c w = withDisplay $ \d -> io $ do
  g <- initColor d c
  whenJust g $ \g' -> setWindowBorder d w g'


-- getWindowRect :: Window -> X Rectangle
-- getWindowRect w = do d <- asks display
--                      bw <- getWindowBorderWidth d w
--                      atts <- io $ getWindowAttributes d w
--                      return $ Rectangle
--                        (fromIntegral $ wa_x atts)
--                        (fromIntegral $ wa_y atts)
--                        (fromIntegral $ bw + wa_width atts)
--                        (fromIntegral $ bw + wa_height atts)

setBorderHook =
  let twoOrMore :: [a] -> Bool
      twoOrMore (_:(_:_)) = True
      twoOrMore _ = False in
  withFocused $ \w -> do
  us <- fmap (not . null) readUrgents
--  rect <- getWindowRect w
--  scr <- gets $ screenRect . W.screenDetail . W.current . windowset
  others <- gets $ twoOrMore . W.integrate' . W.stack . W.workspace . W.current . windowset
  -- this is no good as scr and rect are not right
  -- what I want is to know if there are other windows that are invisible?

  if us then setBorder Local.Theme.hasUrgentBorderColor w
    else if (not others) then setBorder Local.Theme.singletonBorderColor w
    else return ()

addManageRules c = withUrgencyHookC LibNotifyUrgencyHook
                   urgencyConfig { suppressWhen = Focused
                                 , remindWhen = Every 120 }
                   $ c { manageHook = (manageHook c) <+> windowRules
                       , logHook = (logHook c) >> setBorderHook }

windowRules = composeAll
  [ isDialog --> doFloat
  , transience'
  , isFullscreen --> doFullFloat
  , className =? "Xmessage" --> doFloat ]

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
