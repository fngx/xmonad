{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}
import qualified Debug.Trace as DT
import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import XMonad.Util.EZConfig

import XMonad.Util.NamedWindows

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.Place
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks ( ToggleStruts (ToggleStruts) )

import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Renamed as Ren
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Layout.BoringWindows as Boring
import qualified XMonad.Layout.VarialColumn as VC
import XMonad.Layout.LayoutCombinators ( (|||), JumpToLayout (JumpToLayout) )

import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Shell as XPS

import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.DwmPromote as DWM
import XMonad.Actions.FindEmptyWorkspace
import qualified XMonad.Actions.CycleWS as C
import XMonad.Actions.Warp (warpToWindow)
import qualified XMonad.Actions.GridSelect2 as GS
import XMonad.Actions.WindowBringer (bringWindow)

import System.Exit
import Data.List (isInfixOf, (\\))
import Data.Char (toLower)
import Data.Maybe (isJust, fromJust, listToMaybe, fromMaybe)

import XMonad.Layout.CountLabel (addCount)
import XMonad.Util.AccelerateScroll
import XMonad.Actions.BringFrom (bringFrom)

import System.Taffybar.Hooks.PagerHints (pagerHints)

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.ToggleLimit
import qualified Data.Map as M

as n x = Ren.renamed [Ren.Replace n] x

layout = XMonad.Layout.NoBorders.smartBorders $
         addCount $
         mkToggle (single FULL) $
         mkToggle (single (TL 2)) $
         Boring.boringAuto $
         as "Cols" VC.varial

-- bindings which work in certain layouts
inLayout :: [(String, X ())] -> X () -> X ()
inLayout as d =
  do lname <- gets (description . W.layout . W.workspace . W.current . windowset)
     let lname' = head $ words lname
     fromMaybe d $ lookup lname' as

focusUp = inLayout [("Full", windows W.focusUp), ("Limit", rotSlavesUp)] Boring.focusUp
focusDown = inLayout [("Full", windows W.focusDown), ("Limit", rotSlavesDown)] Boring.focusDown

manageHooks config = config {
  manageHook = (manageHook config) <+>
               placeHook (withGaps (16,16,16,16) (underMouse (0.5, 0.2))) <+>
               composeAll
               [
                 isDialog --> doFloat,
                 isFullscreen --> doFullFloat
               ]
  }

eventHooks config = config {
  handleEventHook = (handleEventHook config) <+>
                    fullscreenEventHook
  }

typeKey :: String -> X ()
typeKey k = spawn $ "xdotool key --clearmodifiers " ++ k

minWs = "*"
wsNames = ["q", "w", "e", "r", "t"] ++ [minWs]

interestingWS = C.WSIs $
  do hs <- gets (map W.tag . W.hidden . windowset)
     return (\w -> (W.tag w /= minWs) &&
                   (isJust $ W.stack w) &&
                   (W.tag w `elem` hs))


commandMenu = (GS.runSelectedAction gsconfig
                {
                  GS.gs_cellwidth = 128
                }
                [
                  ("emacs", spawn "emacsclient -c -n"),
                  ("qutebrowser", spawn "qb"),
                  ("hibernate", spawn "systemctl hibernate"),
                  ("suspend", spawn "systemctl suspend"),
                  ("compose mail", spawn "xdg-open mailto:"),
                  ("check mail", spawn "notmuch new"),
                  ("chromium", spawn "chromium"),
                  ("volume control", spawn "pavucontrol"),
                  ("tenrox", spawn "chromium --new-window http://cse.tenrox.net/"),
                  ("lights", spawn "curl http://lights.research.cse.org.uk/toggle")
                ])

expandH :: Rational -> X ()
expandH m = withFocused $ \w -> sendMessage $ VC.Embiggen m 0 w
expandV :: Rational -> X ()
expandV m = withFocused $ \w -> sendMessage $ VC.Embiggen 0 m w

moose = warpToWindow 0.1 0.1

resetLayout = do
  h <- asks (layoutHook . config)
  setLayout h

windowKeys =
  [ ("M-S-k", kill)
  , ("M-M1-k", spawn "xkill")
  , ("M--", windows $ W.shift minWs)
  , ("M-=", bringFrom minWs)
  , ("M-S-=", bringMinned gsconfig)
  , ("M-p", focusUp)
  , ("M-n", focusDown)
  , ("M-S-p", withFocused $ \w -> sendMessage $ VC.UpOrLeft w)
  , ("M-S-n", withFocused $ \w -> sendMessage $ VC.DownOrRight w)
  , ("M-M1-p", rotSlavesUp)
  , ("M-M1-n", rotSlavesDown)
  , ("M-<Return>", DWM.dwmpromote >> moose)
  , ("M-u", bringUrgent)
  , ("M-y", GS.bringSelected gsconfig)
  , ("M-j", goToSelected gsconfig)
  , ("M-S-i", expandH 0.1)
  , ("M-S-o", expandH (-0.1))
  , ("M-i", expandV 0.1)
  , ("M-o", expandV (-0.1))
  , ("M-M1-i", withFocused $ \w -> sendMessage $ VC.GrabColumn w)
  , ("M-M1-o", withFocused $ \w -> sendMessage $ VC.EqualizeColumn 1 w)
  , ("M-z", withFocused $ windows . W.sink)
  , ("M-m", windows $ W.focusMaster)
  , ("M-c",  withFocused $ \w -> sendMessage $ VC.ToNewColumn w)
  ]

workspaceKeys =
  [(mod ++ k, (a ws)) |
    ks <- [["q", "w", "e", "r", "t"], map show [1..9]],
    (k, ws) <- zip ks wsNames,
    (mod, a) <- [("M-", windows . W.greedyView), ("M-S-", windows . W.shift)] ]
  ++
  [ ("M-s", C.toggleWS' [minWs])
  , ("M-g", viewEmptyWorkspace)
  , ("M-S-G", tagToEmptyWorkspace)
  , ("M-d", C.moveTo C.Prev interestingWS)
  , ("M-f", C.moveTo C.Next interestingWS)
  ]

screenKeys =
  [ ("M-v", C.nextScreen)
  , ("M-x", C.swapNextScreen)
  , ("M-S-x", C.shiftNextScreen)
  , ("M-M1-d", C.nextScreen >> C.moveTo C.Prev interestingWS >> C.prevScreen)
  , ("M-M1-f", C.nextScreen >> C.moveTo C.Next interestingWS >> C.prevScreen)
  ]

commandKeys =
  [ ("M-S-<Return>", spawn "xterm")
  , ("M-a", commandMenu)
  , ("M-S-a", XPS.shellPrompt prompt)
  ]

layoutKeys =
  [ ("M-<Space>", sendMessage $ Toggle FULL)
  , ("M-b", sendMessage ToggleStruts)
  , ("M-S-<Space>", resetLayout)
  , ("M-l", sendMessage $ Toggle $ TL 2)
  ]

myKeys =
  [ ("M-S-<Escape>", io (exitWith ExitSuccess))
  , ("M-<Escape>", spawn "xmonad --recompile; xmonad --restart") ]
  ++
  windowKeys
  ++
  workspaceKeys
  ++
  screenKeys
  ++
  commandKeys
  ++
  layoutKeys

main = do
  xmonad $
    withUrgencyHookC BorderUrgencyHook
    { urgencyBorderColor = "cyan" } urgencyConfig { suppressWhen = XMonad.Hooks.UrgencyHook.Never } $
    manageHooks $
    eventHooks $
    pagerHints $
    desktopConfig
    { modMask     = mod4Mask
    , clickJustFocuses = False
    , layoutHook = desktopLayoutModifiers $
                   layout
    , workspaces = wsNames
    , normalBorderColor = "dark slate gray"
    , focusedBorderColor = "dark orange"
    , borderWidth = 2
    , keys = const $ M.empty -- nuke defaults
    }
    `additionalMouseBindings`
    [
      ((0,4), const $ accelerateButton 4),
      ((0,5), const $ accelerateButton 5),
      ((mod4Mask, 4), const $ typeKey "XF86AudioLowerVolume"),
      ((mod4Mask, 5), const $ typeKey "XF86AudioRaiseVolume")
    ]
    `additionalKeysP`
    myKeys
  where
    deltaw :: Rational
    deltaw = 0.2
    deltah :: Rational
    deltah = 0.2

prompt = XP.def
   { XP.font = "xft:Mono-12"
   , XP.height = 28
   , XP.position = XP.Top
   , XP.borderColor = "#AAAAAA"
   , XP.fgColor = "#F5f5f5"
   , XP.bgColor = "#494949"
   , XP.fgHLight = "#000000"
   , XP.bgHLight = "#ffffff"
   , XP.promptBorderWidth = 2
   , XP.searchPredicate = isInfixOf . (map toLower)
   , XP.maxComplRows = Just 10
   , XP.historySize = 100
   , XP.promptKeymap = XP.emacsLikeXPKeymap
 }

gsconfig = GS.def
  {
    GS.gs_navigate = GS.navNSearch,
    GS.gs_cellwidth = 256
  }

goToSelected :: GS.GSConfig (String, Window) -> X ()
goToSelected c = GS.withSelectedWindow (\t _ -> t /= minWs) (windows . W.focusWindow) c

bringMinned :: GS.GSConfig (String, Window) -> X ()
bringMinned = GS.withSelectedWindow (\t _ -> t == minWs) $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

bringUrgent = do
  urg <- readUrgents
  win <- case urg of
    [x] -> return $ Just x
    _ -> GS.gridselectWindow (const $ (flip elem urg)) gsconfig
  whenJust win $ \w -> (windows $ bringWindow w) >> (windows $ W.focusWindow w)
