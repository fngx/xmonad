{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, NoMonomorphismRestriction #-}

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W
import XMonad.Config.Desktop
import XMonad.Util.EZConfig

import XMonad.Util.NamedWindows

import XMonad.Hooks.Place
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks ( ToggleStruts (ToggleStruts) )

import XMonad.Layout.NoBorders
import qualified XMonad.Layout.Renamed as Ren
import qualified XMonad.Layout.MouseResizableTile as MRT
import qualified XMonad.Layout.BoringWindows as Boring
import qualified XMonad.Layout.LimitWindows as Limit
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

import qualified Data.Map as M

as n x = Ren.renamed [Ren.Replace n] x

layout = XMonad.Layout.NoBorders.smartBorders $
         addCount $
         mkToggle (single NBFULL) $
         Boring.boringAuto $ (tiled ||| twocol)
  where
    tiled = as "s" $ VC.varial
    twocol = as "d" $ Limit.limitWindows 2 $ VC.varial

-- bindings which work in certain layouts
inLayout :: [(String, X ())] -> X () -> X ()
inLayout as d =
  do lname <- gets (description . W.layout . W.workspace . W.current . windowset)
     let lname' = head $ words lname
     fromMaybe d $ lookup lname' as

focusUp = inLayout [("Full", windows W.focusUp)] Boring.focusUp
focusDown = inLayout [("Full", windows W.focusDown)] Boring.focusDown

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
wsNames = ["q","w","e","r"] ++ (map show [5..9]) ++ [minWs]

interestingWS = C.WSIs $
  do hs <- gets (map W.tag . W.hidden . windowset)
     return (\w -> (W.tag w /= minWs) &&
                   (isJust $ W.stack w) &&
                   (W.tag w `elem` hs))
main = do
  xmonad $
    manageHooks $
    eventHooks $
    pagerHints $
    desktopConfig
    { modMask     = mod4Mask
    , clickJustFocuses = False
    , layoutHook = desktopLayoutModifiers $
                   layout
    , workspaces = wsNames
    , focusedBorderColor = "#00bfff"
    , borderWidth = 2
    }
    `additionalMouseBindings`
    [
      ((0,4), const $ accelerateButton 4),
      ((0,5), const $ accelerateButton 5),
      ((mod4Mask, 4), const $ typeKey "XF86AudioLowerVolume"),
      ((mod4Mask, 5), const $ typeKey "XF86AudioRaiseVolume")
    ]
    `removeKeysP`
    ([p ++ [n] | p <- ["M-", "M-S-"], n <- ['1'..'9']] ++ ["M-S-c", "M-k"])
    `additionalKeysP`
    ([("M-<Escape>", spawn "xmonad --recompile; xmonad --restart"),
      ("M-S-<Escape>", io (exitWith ExitSuccess)),
      ("M-<Return>", DWM.dwmpromote),

      ("M-h", C.moveTo C.Prev interestingWS),
      ("M-j", C.moveTo C.Next interestingWS),
      ("M-S-y", tagToEmptyWorkspace),
      ("M-y", viewEmptyWorkspace),

      ("M-S-a", XPS.shellPrompt prompt),

      ("M-a",
       (GS.runSelectedAction gsconfig
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
         ("volume control", spawn "pavucontrol")
         ])),

      ("M-S-k", kill),

      ("M-g",   goToSelected gsconfig),
      ("M-b",   GS.bringSelected gsconfig),
      ("M-S-b", bringMinned gsconfig),

      ("M-v",   sendMessage ToggleStruts),

      ("M-m", windows $ W.shift minWs),
      ("M-S-m", bringFrom minWs),

      ("M-p", focusUp),
      ("M-n", focusDown),

      ("M-,", rotSlavesUp),
      ("M-.", rotSlavesDown),

      ("M--",   C.nextScreen >> warpToWindow 0.1 0.1),
      ("M-S--", C.shiftNextScreen >> C.nextScreen >> warpToWindow 0.1 0.1),
      ("M-=",   C.swapNextScreen),

      ("M-S-n", withFocused $ \w -> sendMessage $ VC.DownOrRight w),
      ("M-S-p", withFocused $ \w -> sendMessage $ VC.UpOrLeft w),

      ("M-c M-c", withFocused $ \w -> sendMessage $ VC.ToNewColumn w),
      ("M-c c",   withFocused $ \w -> sendMessage $ VC.GrabColumn w),
      ("M-c h",   withFocused $ \w -> sendMessage $ VC.GrabRow w),
      ("M-c e",   withFocused $ \w -> sendMessage $ VC.EqualizeColumn 1 w),
      ("M-c q",   withFocused $ \w -> sendMessage $ VC.EqualizeColumn 0.5 w),

      ("M-u",   withFocused $ \w -> sendMessage $ VC.Embiggen deltaw 0 w),
      ("M-S-u", withFocused $ \w -> sendMessage $ VC.Embiggen (-deltaw) 0 w),
      ("M-i",   withFocused $ \w -> sendMessage $ VC.Embiggen 0 deltah w),
      ("M-S-i", withFocused $ \w -> sendMessage $ VC.Embiggen 0 (-deltah) w),

      ("M-f", sendMessage $ Toggle NBFULL)
     ]
     ++
     [("M-" ++ k, ((sendMessage $ JumpToLayout k))) | k <- ["s","d"]]
     ++
     [(mod ++ k, (a ws)) |
       ks <- [["q", "w", "e", "r"], map show [1..9]],
       (k, ws) <- zip ks wsNames,
       (mod, a) <- [("M-", windows . W.greedyView), ("M-S-", windows . W.shift)] ]
    )
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

goToSelected :: GS.GSConfig Window -> X ()
goToSelected c = GS.withSelectedWindow (\t -> t /= minWs) (windows . W.focusWindow) c

bringMinned :: GS.GSConfig Window -> X ()
bringMinned = GS.withSelectedWindow (\t -> t == minWs) $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster
