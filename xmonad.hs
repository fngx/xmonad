{-# LANGUAGE FlexibleContexts #-}

import Control.Monad (liftM2)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Exit (exitWith, ExitCode (ExitSuccess))
import System.Taffybar.Hooks.PagerHints (pagerHints)
import XMonad hiding (config)
import XMonad.Actions.CycleWS
import XMonad.Actions.Iconify (greedyFocusWindow, focusWindow, iconify, uniconify)
import XMonad.Actions.Search
import XMonad.Actions.WindowBringer (bringWindow)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook, ewmh)
import XMonad.Hooks.ManageDocks ( avoidStruts, docksEventHook, docksStartupHook, manageDocks )
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat, doSideFloat, Side(..), transience')
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Groups.Helpers as G
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt.MyPrompt
import XMonad.Util.AccelerateScroll (accelerateButton)
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.HintedSubmap (hintSubmap)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn, spawnPipe, hPutStrLn, runProcessWithInput)
import XMonad.Util.TemporaryBar
import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad
import qualified XMonad.Actions.Ring as Ring
import qualified XMonad.Layout.Rows as R
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.XMobar (runWithBar)
import XMonad.Prompt.WindowPrompt2 (windowPrompt, WindowPrompt (..))
import XMonad.Actions.WindowGo (runOrRaiseAndDo)
import XMonad.Hooks.FadeInactive (setOpacity)
import XMonad.Actions.CopyWindow
import XMonad.Layout.TrackFloating (trackFloating)

import qualified XMonad.Util.Colours as Cs

term = "urxvt"

main = runWithBar config

popBar = tempShowBar 0.75

wsLabels = ["1", "2", "3", "4", "5"]
icon = "▼"

resetLayout = do
  layout <- asks (layoutHook . XMonad.config)
  setLayout layout

layout c = c
  { layoutHook = l }
  where l = trackFloating $ avoidStruts $ smartBorders $ (R.rows ||| Full)

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        whenX (fmap not $ runQuery (className =? "qutebrowser") w) $ do
          name     <- getName w
          Just idx <- fmap (W.findTag w) $ gets windowset
          safeSpawn "notify-send"
            ["urgent: " ++ (show name), "-a", "urgency"]
        withDisplay $ \d -> io $ do
          c' <- initColor d Cs.urgent
          case c' of
            Just c -> setWindowBorder d w c
            _ -> return ()

hooks c =
  withUrgencyHookC LibNotifyUrgencyHook
  urgencyConfig {suppressWhen = Focused}
  $ ewmh $
  c
  { handleEventHook =  toggleBarHook <+> docksEventHook <+> fullscreenEventHook <+> (handleEventHook c)
  , manageHook = manageDocks <+>
                 (manageHook c) <+>
                 composeAll
                 [ isDialog --> doFloat
                 , isFullscreen --> doFullFloat
                 , title =? "xclock" --> (doSideFloat NE) <+>
                   (ask >>= \w -> liftX (setOpacity w 0.5) >> idHook)
                 , transience'
                 ]
  , logHook = (logHook c) >> (Ring.update $ fmap (liftM2 (,) W.peek W.allWindows) (gets windowset))
  , startupHook = setDefaultCursor xC_left_ptr <+> docksStartupHook <+> (startupHook c)
  }

config =
  pagerHints $
  hooks $
  layout $
  flip additionalMouseBindings
  [((0, button4), const $ accelerateButton 4)
  ,((0, button5), const $ accelerateButton 5)

  ,((mod4Mask, button4), const $ R.shrinkFocusedColumn)
  ,((mod4Mask, button5), const $ R.growFocusedColumn)

  ,((mod4Mask .|. shiftMask, button4), const $ R.shrinkFocusedRow)
  ,((mod4Mask .|. shiftMask, button5), const $ R.growFocusedRow)
  ]
  $
  flip additionalKeysP bindings $
  def
  { modMask = mod4Mask
  , workspaces = wsLabels ++ [icon]
  , keys = const $ M.empty
  , normalBorderColor  = Cs.dimBorder
  , focusedBorderColor = Cs.border
  , borderWidth = 1
  , terminal = term
}

-- there is a bug in !>, it can't combine multiple prefixes
mySearchEngine = intelligent $ searchEngineF (L.intercalate "," $ map fst se) msearch
  where se :: [(String, SearchEngine)]
        se = [ ("wp", wikipedia)
             , ("osm", openstreetmap)]
        msearch :: String -> String
        msearch s = let p :: String
                        p = fst $ break (==':') s

                        ddg :: SearchEngine
                        ddg = prefixAware $ searchEngine "ddg" "https://duckduckgo.com/?q="
                        sel :: SearchEngine
                        sel = fromMaybe ddg $ lookup p se
                    in (use sel) s

resize = hintSubmap config
  [ ("e", "taller", R.growFocusedRow >> resize)
  , ("d", "shorter", R.shrinkFocusedRow >> resize)
  , ("s", "narrower", R.shrinkFocusedColumn >> resize)
  , ("f", "wider", R.growFocusedColumn >> resize)
  , ("r", "reset", do R.resetRow
                      R.resetColumn
                      R.resetRow
                      R.resetColumn)]

volume = let p = reverse . (drop 1) . reverse in
           do cv <- fmap p $ runProcessWithInput "pamixer" ["--get-volume"] ""
              cm <- fmap p $ runProcessWithInput "pamixer" ["--get-mute"] ""
              let cmi = read cv :: Int
                  ifl x y = if x then [y] else []
              hintSubmap config $
                [ ("x", "mixer", spawn "pavucontrol")
                , ("m", if (cm == "true") then
                      "unmute" else "mute", (spawn "pamixer -t") >> volume) ]
                ++ (ifl (cmi > 0) ("q", "quieter [" ++ (show $ cmi - 5) ++ "%]",
                                   (runProcessWithInput "pamixer" ["-d", "5"] "") >> volume))
                ++ (ifl (cmi < 100) ("l", "louder [" ++ (show $ cmi + 5) ++ "%]",
                                     (runProcessWithInput "pamixer" ["-i", "5"] "") >> volume))
                ++ [(k, p++"%", (runProcessWithInput "pamixer" ["--set-volume", p] "") >> return ()) |
                    (k, p) <- [("y", "10"), ("u", "20"), ("i", "30"), ("o", "40"), ("p","50")]]


mainBindings =
  [ ("<Escape>", "session",
      hintSubmap config
      [ ("r", "restart xmonad", spawn "xmonad --recompile; xmonad --restart")
      , ("q", "exit xmonad", io (exitWith ExitSuccess))
      , ("l", "reset layout", resetLayout)
      , ("h", "hibernate", spawn "systemctl hibernate")
      , ("b", "reboot", spawn "systemctl reboot")
      , ("p", "poweroff", spawn "systemctl poweroff")
      , ("s", "suspend", spawn "systemctl suspend")
      ])

  -- keys to launch programs
  , ("<Return>", "terminal", spawn term)

  , ("a", "run keys",
     hintSubmap config
     [ ("e", "emacs", spawn "emacsclient -c -n")
     , ("q", "web search", promptSearchBrowser pconfig "/home/hinton/bin/qb" mySearchEngine)
     , ("M-q", "search sel", selectSearchBrowser "/home/hinton/bin/qb" mySearchEngine)

     , ("w w", "w3m", spawn "emacsclient -c -n -e '(w3m)'")
     , ("w q", "qutebrowser", spawn "qb")
     , ("w c", "chromium", spawn "chromium")

     , ("t", "htop", spawn $ term ++ " -e htop")
     , ("u", "cmus", spawn $ term ++ " -e cmus")

     , ("r", "prompt", shell)
     , ("v", "vol", volume)
     , ("c", "clock", runOrRaiseAndDo "xclock" (title =? "xclock") killWindow)
     ])

  , ("v", "volume", volume)
  , ("c", "prompt", shell)

  -- keys to adjust the stack and focus
  , ("k", "kill window", kill)

  -- keys to adjust the layout

  , ("p",   "focus up", R.focusPrev)
  , ("n",   "focus down", R.focusNext)

  , ("M1-p", "group up", R.prevGroup)
  , ("M1-n", "group down", R.nextGroup)

  , ("S-p", "swap up", R.swapPrev)
  , ("S-n", "swap down", R.swapNext)

  , ("l", "layout keys",
     hintSubmap config
      [ ("l", "group layout", R.groupNextLayout)
     , ("M-l", "ditto", R.groupNextLayout)
     , ("o", "new group", R.makeGroup)
     , ("R", "reset layout", resetLayout)
     , ("f", "fullscreen col", popBar >> R.outerNextLayout)
     , ("x", "maximize window", R.toggleWindowFull)
     , ("b", "balance?", sendMessage R.BalanceToggle)
     , ("r", "resize", resize)
     , ("z", "sink window", withFocused $ windows . W.sink)
     , ("s", "sticky window", windows copyToAll)
     ])

  , ("'", "next layout", R.groupNextLayout)

  -- , ("d", "up", R.prevInGroup)
  -- , ("c", "down", R.nextInGroup)
  -- , ("s", "left", R.prevGroup)
  -- , ("", "right", R.nextGroup)

  , ("<Tab>", "Cycle window", R.nextInGroup)
  , ("M1-<Tab>", "Cycle group", R.nextGroup)

  , ("o", "pop", R.makeGroup)
  , (".", "resize", resize)
  , ("=", "balance", sendMessage R.BalanceToggle)

  -- this makes the outerlayout fullscreen

  , ("f", "fullscreen", sendMessage NextLayout)

  -- minify
  , ("m", "minify", popBar >> iconify icon)
  , (",", "unminify", popBar >> uniconify icon)

  -- cycle and unminify
  , ("<Space>", "cycle focus",
     do us <- readUrgents
        case us of
          (h:_) -> windows $ focusWindow icon h
          [] -> Ring.rotate [xK_Super_L] xK_space (windows . (focusWindow icon))
        resetBar)

    -- one day I shall replace dmenu perhaps

  , ("b", "bring window",
     windowPrompt (WindowPrompt "bring window: ") qconfig >>=
     \w -> maybe (return ()) (windows.bringWindow) w)

  , ("g", "find window",
     windowPrompt (WindowPrompt "find window: ") qconfig >>=
     \w -> maybe (return ()) (windows . (focusWindow icon)) w)

  , (";", "toggle bar", toggleBar)
  , ("S-;", "show bar", tempShowBar 2)

  , ("s", "swap screen", popBar >> swapNextScreen)
  , ("S-s", "shift screen", popBar >> shiftNextScreen)
  , ("M1-s", "focus screen", popBar >> nextScreen)
  , ("x", "focus screen", popBar >> nextScreen)

  , ("S-/", "this page", hintSubmap config mainBindings)]

bindings = (map (\(k, _, a) -> ("M-" ++ k, a)) mainBindings)
  ++
   --laptop keys
  [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  , ("<XF86ScreenSaver>", spawn "systemctl hibernate")
  ]
  ++
  -- workspace switching keys
  [ (mod ++ key, action ws) |
    (ws, key) <- (zip wsLabels ["q", "w", "e", "r", "t"]),
    (mod, action) <- [ ("M-", \t -> (windows $ W.greedyView t) >> popBar)
                     , ("M-S-", \t -> (windows $ W.shift t) >> popBar)
                     , ("M-M1-", \t -> (windows $ lazyView t) >> popBar)
                     ] ]

lazyView :: (Eq s, Eq i) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
lazyView i s@(W.StackSet { W.hidden = _:_ })
  -- if i is hidden, then we want to raise it on the next visible screen, if there is one
  | Just x <- L.find ((i==) . W.tag) (W.hidden s) =
      let (v1:vs) = W.visible s in
        s { W.visible = (v1 { W.workspace = x }):vs
          , W.hidden = W.workspace v1 : L.deleteBy (equating W.tag) x (W.hidden s) }
  | otherwise = W.view i s
  where equating f = \x y -> f x == f y
