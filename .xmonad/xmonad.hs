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
import XMonad.Actions.WindowBringer
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks ( ToggleStruts (ToggleStruts) )
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doFullFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Groups.Helpers as G
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Prompt.MyPrompt
import XMonad.Prompt.Pass
import XMonad.Prompt.NetworkManager (nmPrompt)
import XMonad.Util.AccelerateScroll (accelerateButton)
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.HintedSubmap (hintSubmap)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (safeSpawn, spawnPipe, hPutStrLn)
import XMonad.Util.TemporaryBar
import qualified Data.List as L
import qualified Data.Map as M
import qualified XMonad
import qualified XMonad.Actions.Ring as Ring
import qualified XMonad.Layout.Rows as R
import qualified XMonad.StackSet as W
import XMonad.Util.XMobar (runWithBar)

main = runWithBar config

popBar = tempShowBar 0.75

wsLabels = ["q", "w", "e", "r", "t"]
icon = "▼"

resetLayout = do
  layout <- asks (layoutHook . XMonad.config)
  setLayout layout

layout c = c
  { layoutHook = l }
  where l = desktopLayoutModifiers $ smartBorders $ (R.rows (focusedBorderColor c) ||| Full)

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name     <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" ["urgent: " ++ (show name)]
        withDisplay $ \d -> io $ do
          c' <- initColor d "red"
          case c' of
            Just c -> setWindowBorder d w c
            _ -> return ()

hooks c =
  withUrgencyHookC LibNotifyUrgencyHook
  urgencyConfig {suppressWhen = Never}
  $
  c
  { handleEventHook = (handleEventHook c) <+> fullscreenEventHook <+>
    toggleBarHook
  , manageHook = (manageHook c) <+>
                 composeAll
                 [ isDialog --> doFloat,
                   isFullscreen --> doFullFloat
                 ]
  , logHook = (logHook c) >> (Ring.update $ fmap (liftM2 (,) W.peek W.allWindows) (gets windowset))
  , startupHook = (startupHook c)
  }

config =
  pagerHints $
  hooks $
  layout $
  flip additionalMouseBindings
  [((0, button4), const $ accelerateButton 4)
  ,((0, button5), const $ accelerateButton 5)]
  $
  flip additionalKeysP (map (\(k, _, a) -> ("M-" ++ k, a)) bindings) $
  desktopConfig
  { modMask = mod4Mask
  , workspaces = wsLabels ++ [icon]
  , keys = const $ M.empty
  , normalBorderColor  = "#888888"
  , focusedBorderColor = "darkorange"
  , borderWidth = 2
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

bindings =
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
  , ("S-<Return>", "terminal", spawn "xterm")

  , ("-", "show bar", tempShowBar 2)

  , ("a", "run keys",
     hintSubmap config
     [ ("e", "emacs", spawn "emacsclient -c -n")
     , ("q", "web search", promptSearchBrowser pconfig "/home/hinton/bin/qb" mySearchEngine)
     , ("M-q", "search sel", selectSearchBrowser "/home/hinton/bin/qb" mySearchEngine)

     , ("w w", "qutebrowser", spawn "qb")
     , ("w c", "chromium", spawn "chromium")

     , ("p", "passwords", passwordPrompt pconfig)
     , ("t", "htop", spawn "xterm -e htop")
     , ("m", "check mail", spawn "notmuch new")
     , ("u", "cmus", spawn "xterm -e cmus")

     , ("r", "prompt", shell)
     , ("n", "network", nmPrompt "up")
     ])


  -- keys to adjust the stack and focus
  , ("k", "kill window", kill)

  -- keys to adjust the layout
  , ("z", "sink window", withFocused $ windows . W.sink)

  , ("p",   "focus up", R.focusPrev)
  , ("n",   "focus down", R.focusNext)
  , ("S-p", "swap up", R.swapPrev)
  , ("S-n", "swap down", R.swapNext)

  , ("l", "layout keys",
     hintSubmap config
     [ ("l", "switch group layout", R.groupNextLayout)
     , ("M-l", "ditto", R.groupNextLayout)
     , ("o", "focused to new group", R.makeGroup)
     , ("m", "max window", R.maximize)
     , ("e", "eq windows", R.equalize)
     , ("r", "reset layout", resetLayout)
     , ("w", "max col", R.maximizeC)
     , ("n", "eq col", R.equalizeC)
     , ("f", "fullscreen col", popBar >> R.outerNextLayout)
     ]
     )

  , ("<Return>","swap master", G.swapGroupMaster)

  -- this makes the outerlayout fullscreen

  , ("f", "fullscreen", sendMessage NextLayout)

  -- minify
  , ("m", "minify", popBar >> iconify icon)
  , (",", "unminify", popBar >> uniconify icon)
--  , (".", "cycle min", popBar >> (withFocused $ (\w -> cyclew icon w)))

  -- cycle and unminify
  , ("<Space>", "cycle focus",
     do us <- readUrgents
        case us of
          (h:_) -> windows $ greedyFocusWindow icon h
          [] -> Ring.rotate [xK_Super_L] xK_space (windows . (focusWindow icon))
        resetBar)

    -- one day I shall replace dmenu perhaps

  , ("b", "bring window", bringMenuArgs ["-i", "-l", "10", "-p", "bring"])
  , ("g", "find window", gotoMenuArgs  ["-i", "-l", "10", "-p", "goto"])
  , (";", "toggle bar", toggleBar)

  , ("s", "swap screen", popBar >> swapNextScreen)
  , ("S-s", "shift screen", popBar >> shiftNextScreen)
  , ("M1-s", "focus screen", popBar >> nextScreen)

  , ("S-/", "this page", hintSubmap config bindings)]

  ++
  -- workspace switching keys
  [ (mod ++ key, dsc ++ key, action key) |
    key <- wsLabels,
    (mod, dsc, action) <- [ ("", "greedy view ", \t -> (windows $ W.greedyView t) >> popBar)
                          , ("S-", "shift ", \t -> (windows $ W.shift t) >> popBar)
                          , ("M1-", "view ", \t -> (windows $ lazyView t) >> popBar)
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
