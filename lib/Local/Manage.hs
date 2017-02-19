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
import qualified Data.Map.Strict as M
import qualified Debug.Trace as D
import qualified XMonad.Util.ExtensibleState as XS

import Local.Windows (recentWindows, nextInHistory)
import Data.Maybe (listToMaybe, maybeToList)

setBorder c w = withDisplay $ \d -> io $ do
  g <- initColor d c
  whenJust g $ \g' -> setWindowBorder d w g'

setBorderWidth b w = withDisplay $ \d -> io $ do setWindowBorderWidth d w b

data BorderColors = BorderColors (M.Map Window String)
  deriving (Read, Show, Typeable)

instance ExtensionClass BorderColors where
  initialValue = BorderColors $ M.empty

changeBorderColors :: M.Map Window String -> X ()
changeBorderColors new = do
  BorderColors old <- XS.get
  mapM_ (setBorder Local.Theme.normalBorderColor) $
    M.keys $ M.difference old new

  let mSetBorder w c
        | Just c == M.lookup w old = return ()
        | otherwise = setBorder c w

  -- this would be better as a difference operation
  mapM_ (uncurry mSetBorder) $ M.toList new
  XS.put $ BorderColors new

setBorderHook =
  do us <- readUrgents
     nextM <- nextInHistory
     focus <- gets (W.peek . windowset)

     let ucs = map (flip (,) Local.Theme.urgentBorderColor) us
         ncs = (flip (,) Local.Theme.otherWindow) <$> nextM
         fbc = if null us
               then Local.Theme.focusedBorderColor
               else Local.Theme.hasUrgentBorderColor
         fcs = (flip (,) fbc) <$> focus

     changeBorderColors $ M.fromList $
       ucs ++ maybeToList ncs ++ maybeToList fcs

addManageRules c = withUrgencyHookC LibNotifyUrgencyHook
                   urgencyConfig { suppressWhen = Focused
                                 , remindWhen = Every 120 }
                   $ c { manageHook = (manageHook c) <+> windowRules
                       -- this is run on every state update
                       -- when things send a message, which sucks rather
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
