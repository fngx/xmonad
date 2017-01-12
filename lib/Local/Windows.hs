 {-# LANGUAGE DeriveDataTypeable, BangPatterns #-}

module Local.Windows (addHistory, recentWindows, windowKeys, greedyFocusWindow, lastFocus) where

import Control.Monad
import Data.List
import Data.Maybe
import Local.Util
import Local.Workspaces (warp)
import XMonad
import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows (getName, unName, NamedWindow)
import qualified Local.Theme as Theme
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Foldable
import XMonad.Util.NamedWindows (getName)

import XMonad.Util.XUtils
import XMonad.Util.Font

data WindowHistory = WH (Maybe Window) (Seq Window)
  deriving (Typeable, Read, Show)

instance ExtensionClass WindowHistory where
  initialValue = WH Nothing empty
  extensionType = PersistentExtension

updateHistory :: WindowHistory -> X WindowHistory
updateHistory (WH oldcur oldhist) = withWindowSet $ \ss -> do
  let newcur   = W.peek ss
      wins     = Set.fromList $ W.allWindows ss
      newhist  = Seq.filter (flip Set.member wins) (ins oldcur oldhist)
  return $ WH newcur (del newcur newhist)
  where
    ins x xs = maybe xs (<| xs) x
    del x xs = maybe xs (\x' -> Seq.filter (/= x') xs) x

recentWindows :: X [Window]
recentWindows = do
  hook
  (WH cur hist) <- XS.get
  return $ (maybeToList cur) ++ (toList hist)

lastFocus = do
  rws <- recentWindows
  return $ case rws of
             (h:t) -> listToMaybe $ delete h t
             _ -> Nothing

greedyFocusWindow w s | Just w == W.peek s = s
                      | otherwise = fromMaybe s $ do
                          n <- W.findTag w s
                          return $ until ((Just w ==) . W.peek) W.focusUp $ W.greedyView n s

windowKeys = [ ("M-o", ("last focus", oldWindow 1))
             , ("M-S-o", ("second last focus", oldWindow 2))
             ]

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) (x:_) 0 = Just $ x
(!!?) (_:xs) n = xs !!? (n - 1)

oldWindow n = do us <- readUrgents
                 if Data.List.null us then
                   do rws <- recentWindows
                      whenJust (rws !!? n) (windows . W.focusWindow)
                   else focusUrgent
                 warp

addHistory c = c { logHook = hook >> (logHook c) }

hook :: X ()
hook = XS.get >>= updateHistory >>= XS.put

jump :: X ()
jump = withWindowSet $ \ss ->
  let visible' :: [Window]
      visible' = concatMap (W.integrate' . W.stack . W.workspace) $ ((W.current ss):(W.visible ss))
      keys' = [ "h", "j", "k", "l", "a", "s", "d", "f", "q", "w", "e", "r", "z", "x", "c", "v", "b"]
      (keys, visible) = unzip $ Data.List.zip keys' visible'
  in
    do rects <- mapM getWindowRect visible
       names <- mapM (fmap show . getName) visible :: X [String]
       font <- initXMF "xft:Monospace-16"

       let labels = Data.List.zipWith (\x y -> x ++ ": " ++ y) keys names

       d <- asks display
       widths <- mapM (textWidthXMF d font) labels

       -- render all the windows
       -- grab keys and read one key
       -- delete all the windows

       releaseXMF font

  -- get their rectangles and names
  -- make rectangles to display labels in
  -- bind keys
  -- etc.

getWindowRect :: Window -> X Rectangle
getWindowRect w = do d <- asks display
                     atts <- io $ getWindowAttributes d w
                     let bw = wa_border_width atts
                     return $ Rectangle
                       (fromIntegral $ wa_x atts)
                       (fromIntegral $ wa_y atts)
                       (fromIntegral $ bw + wa_width atts)
                       (fromIntegral $ bw + wa_height atts)
