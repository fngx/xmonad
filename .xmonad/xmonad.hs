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
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Layout.BoringWindows as Boring
import qualified XMonad.Layout.VarialColumn as VC
import XMonad.Layout.LayoutCombinators ( (|||), JumpToLayout (JumpToLayout) )

import qualified XMonad.Prompt as XP
import qualified XMonad.Prompt.Shell as XPS
import qualified XMonad.Prompt.XMonad as XPX
import qualified XMonad.Prompt.Window as XPW
import qualified XMonad.Prompt.Pass as XPP

import qualified XMonad.Actions.CycleWindows as CW
import qualified XMonad.Actions.DwmPromote as DWM
import XMonad.Actions.FindEmptyWorkspace
import qualified XMonad.Actions.CycleWS as C
import XMonad.Actions.Warp (warpToWindow)

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

layout = XMonad.Layout.NoBorders.smartBorders $
         Boring.boringAuto $
         addCount $
         mkToggle (single FULL) $
         mkToggle (single (TL 2)) $
         VC.varial

-- bindings which work in certain layouts
inLayout :: [(String, X ())] -> X () -> X ()
inLayout as d =
  do lname <- gets (description . W.layout . W.workspace . W.current . windowset)
     let lname' = head $ words lname
     fromMaybe d $ lookup lname' as

focusUp = inLayout [ ("Full", windows W.focusUp) ]
          Boring.focusUp
focusDown = inLayout [ ("Full", windows W.focusDown) ]
            Boring.focusDown

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

interestingWS = C.WSIs $ do
  hs <- gets (map W.tag . W.hidden . windowset)
  return (\w ->  (W.tag w /= minWs) && (W.tag w `elem` hs) && (isJust $ W.stack w))

onOtherScreen x = C.nextScreen >> x >> C.prevScreen

commands = [ ("emacs", spawn "emacsclient -c -n"),
             ("qutebrowser", spawn "qb"),
             ("hibernate", spawn "systemctl hibernate"),
             ("suspend", spawn "systemctl suspend"),
             ("compose mail", spawn "xdg-open mailto:"),
             ("check mail", spawn "notmuch new"),
             ("chromium", spawn "chromium"),
             ("volume control", spawn "pavucontrol"),
             ("tenrox", spawn "chromium --new-window http://cse.tenrox.net/"),
             ("lights", spawn "curl http://lights.research.cse.org.uk/toggle"),
             ("HALT", spawn "systemctl poweroff"),
             ("pass", XPP.passwordPrompt prompt)
             ]

autoPrompt = prompt
             {
               XP.alwaysHighlight = True,
               XP.autoComplete = Just 500
             }

commandMenu = XPX.xmonadPromptC commands autoPrompt

expandH :: Rational -> X ()
expandH m = withFocused $ \w -> sendMessage $ VC.Embiggen m 0 w
expandV :: Rational -> X ()
expandV m = withFocused $ \w -> sendMessage $ VC.Embiggen 0 m w

moose = warpToWindow 0.1 0.1

resetLayout = do
  h <- asks (layoutHook . config)
  setLayout h

toggle' = C.toggleWS' [minWs]
toggleUrgent = withUrgents f where
  f [] = toggle'
  f (h:_) = windows $ W.focusWindow h

windowKeys =
  [ ("M-S-k", kill)
  , ("M-M1-k", spawn "xkill")
  , ("M-m", windows $ W.shift minWs)
  , ("M-,", bringFrom minWs)
--  , ("M-.", bringMinned gsconfig)
  , ("M-p", focusUp)
  , ("M-n", focusDown)
  , ("M-M1-p", CW.rotUnfocusedUp)
  , ("M-M1-n", CW.rotUnfocusedDown)
  , ("M-;", CW.rotUnfocusedDown)
  , ("M-S-;", CW.rotFocusedDown)
  , ("M-S-p", withFocused $ \w -> sendMessage $ VC.UpOrLeft w)
  , ("M-S-n", withFocused $ \w -> sendMessage $ VC.DownOrRight w)
  , ("M-<Return>", DWM.dwmpromote >> moose)
  , ("M-u", toggleUrgent)
  , ("M-S-u", clearUrgents)
  , ("M-y", XPW.windowPromptBring prompt)
  , ("M-j", XPW.windowPromptGoto prompt)
  , ("M-S-i", expandH 0.1)
  , ("M-S-o", expandH (-0.1))
  , ("M-i", expandV 0.1)
  , ("M-o", expandV (-0.1))
  , ("M-M1-i", withFocused $ \w -> sendMessage $ VC.GrabColumn w)
  , ("M-M1-o", withFocused $ \w -> sendMessage $ VC.EqualizeColumn 1 w)
  , ("M-z", withFocused $ windows . W.sink)
  , ("M-/",  withFocused $ \w -> sendMessage $ VC.ToNewColumn w)
  ]

workspaceKeys =
  [(mod ++ k, (a ws)) |
    ks <- [["q", "w", "e", "r", "t"], map show [1..9]],
    (k, ws) <- zip ks wsNames,
    (mod, a) <- [("M-", windows . W.greedyView),
                 ("M-S-", windows . W.shift),
                 ("M-M1-", \x -> (onOtherScreen$ windows $ W.greedyView x))
                ] ]
  ++
  [ ("M-s", toggle')
  , ("M-g", viewEmptyWorkspace)
  , ("M-S-g", tagToEmptyWorkspace)
  , ("M-d", C.moveTo C.Prev interestingWS)
  , ("M-f", C.moveTo C.Next interestingWS)
  ]

screenKeys =
  [ ("M-c", C.nextScreen)
  , ("M-S-c", C.shiftNextScreen)
  , ("M-x", C.swapNextScreen)
  , ("M-M1-d", onOtherScreen $ C.moveTo C.Prev interestingWS)
  , ("M-M1-f", onOtherScreen $ C.moveTo C.Next interestingWS)
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
  , ("M--", sendMessage VC.FewerColumns)
  , ("M-=", sendMessage VC.MoreColumns)
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
    , normalBorderColor = "gray50"
    , focusedBorderColor = "orange"
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
   , XP.searchPredicate = \x y -> x == "" || (x `isInfixOf` (map toLower y))
   , XP.maxComplRows = Just 10
   , XP.historySize = 100
   , XP.promptKeymap = XP.emacsLikeXPKeymap
 }
