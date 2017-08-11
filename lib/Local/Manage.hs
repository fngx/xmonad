{-# LANGUAGE FlexibleContexts #-}
module Local.Manage (addManageRules) where

import qualified Local.Colors as Colors

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
import Data.List ( (\\) )
import qualified XMonad.Util.ExtensibleState as XS
import Local.Windows (nextInHistory)
import Data.Maybe (listToMaybe, maybeToList)
import Data.IORef
import Local.Theme (resetStyles, styleWindows, urgentStyle, nextStyle, overflowStyle,
                    singleSelectionStyle, multiSelectionStyle)

import qualified XMonad.Actions.TagWindows as T

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
  XS.put $ BorderColors new
  focus <- gets (W.peek . windowset)

  mapM_ (setBorder Colors.normalBorderColor) $
    M.keys $ M.difference old new

  let mSetBorder w c
        | Just w == focus = setBorder c w
        | Just c == M.lookup w old = return ()
        | otherwise = setBorder c w

  mapM_ (uncurry mSetBorder) $ M.toList new

setBorderHook =
  do us    <- readUrgents
     focus <- gets (W.peek . windowset)
     fref  <- io $ newIORef ([] :: [Window])
     nextM <- nextInHistory

     T.withTaggedGlobal "overflow" $ \x -> io $ modifyIORef fref ((:) x)

     over  <- io $ readIORef fref
     io $ writeIORef fref []
     T.withTaggedGlobal "S" $ \x -> io $ modifyIORef fref ((:) x)
     swaps  <- io $ readIORef fref

     let (selectionStyle, selectionColor) =
           case swaps of
             [_] -> (singleSelectionStyle, Colors.singleSelectionColor)
             _ -> (multiSelectionStyle, Colors.multiSelectionColor)

     resetStyles
     styleWindows us urgentStyle
     whenJust nextM $ \next -> do styleWindows [next] nextStyle
     styleWindows (over \\ maybeToList focus) overflowStyle
     styleWindows swaps selectionStyle

     let ucs = map (flip (,) Colors.urgentBorderColor) us
         scs = map (flip (,) selectionColor) swaps
         fbc = Colors.focusedBorderColor
         fcs = (flip (,) fbc) <$> focus
         ocs = map (flip (,) Colors.overflowWindow) over

     changeBorderColors $ M.fromList $
       ocs ++ maybeToList fcs ++ ucs ++ scs

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
  , className =? "Yad" --> doFloat
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

        setBorder Colors.urgentBorderColor w
