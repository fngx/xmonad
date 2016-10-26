module Local.Prompts (promptKeys, promptsLogHook, rotWindow, Local.Ring.RingDirection (..)) where

import qualified Local.Theme as Theme
import qualified Local.Ring
import Local.Prompt
--import Local.Workspaces
import XMonad.Prompt.Shell (getCommands)
import Data.List
import XMonad (spawn, io, windows, gets, windowset, X, Window, withFocused, whenJust, runQuery,
               className)
import XMonad.Operations (killWindow)
import XMonad.Util.Run
import XMonad.Util.NamedWindows (getName, unName, NamedWindow)
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowBringer (bringWindow)
import Data.Char (toLower)
import XMonad.Actions.DynamicWorkspaces
import Control.Monad (liftM2)
import XMonad.Actions.WithAll (killAll)
import Data.Ratio ((%))

-- uses my prompt utility to provide a few prompts

myConfig = Config
           { normal = ("white", "#222")
           , item = (Theme.normalText, "#444")
           , highlight = (Theme.secondaryText, Theme.secondaryColor)
           , border = (1, Theme.secondaryColor)
           , font = "xft:Liberation Sans-12:bold"
           , prompt = ":"
           , keymap = [ ("<Escape>", promptClose)
                      , ("C-g", promptClose)

                      , ("<Return>", promptDone)

                      , ("M-<Return>", promptNextAction)

                      , ("<Backspace>", promptBackspace)
                      , ("C-h", promptBackspace)
                      , ("C-d", promptDel)
                      , ("C-b", promptCursorLeft)
                      , ("C-f", promptCursorRight)
                      ]
                      ++
                      [(k, promptNextOption) | k <- ["C-n", "<Tab>", "C-i", "<Right>"]]
                      ++
                      [(k, promptPrevOption) | k <- ["C-p", "S-<Tab>", "C-S-I", "<Left>"]]
           , rawKeymap = [ ((1, 65056), promptPrevOption)] -- shift tab?
           }

runPrompt =
  let actions c = (c, [("run", io $ spawn c),
                       ("term", io $ safeSpawn "urxvt" ["-e", c]),
                       ("man", io $ safeSpawn "xterm" ["-e", "man " ++ c]),
                       ("dzn", io $ spawn $ c ++ "|dzen2 -p 3")
                      ])
      generate s = do commands <- io $ getCommands
                      let matches = if null s then commands
                                    else let commands' = (filter (isPrefixOf s) commands) in
                                           if (' ' `elem` s) then s:commands' else commands'
                      return $ map actions matches
  in
    select myConfig { prompt = "cmd: "
                    , keymap = (keymap myConfig) ++ [("<Tab>", promptComplete), ("<Space>", promptAppendSpace)]
                    } generate


-- TODO trim window names
-- TODO maybe indicate ws for windows
windowPrompt =
  let actions :: (NamedWindow, String) -> (String, [(String, X ())])
      actions (nw, c) = let w = unName nw in (show nw ++ " [" ++ c ++ "]",
                                              [ ("go", windows $ W.focusWindow w)
                                              , ("bring", windows $ bringWindow w)
                                              , ("close", killWindow w)] )

      shorten n s
        | len > n = start ++ elip ++ end
        | otherwise = s
        where
          len = length s
          n' = fromIntegral $ ceiling (n % 2)
          start = take n' s
          end = drop (len - n') s
          elip = "…"


      getWindows :: X [Window]
      getWindows = do (f, rc) <- Local.Ring.contents
                      ws <- (fmap W.allWindows (gets windowset))

                      return $ rc ++ (ws \\ rc)
      generate s = do named <- getWindows >>= mapM (\x -> do n <- getName x
                                                             c <- runQuery className x
                                                             return (n, c))
                      return $ map (\(n, a) -> (shorten 38 n, a)) $ filter ((isInfixOf s) . (map toLower) . fst) $ map actions named
  in
    select myConfig { prompt = "win: "
                    , keymap = ("M-e", promptNextOption):(keymap myConfig)
                    } generate

swap2 (a:(b:cs)) = b:(a:cs)
swap2 x = x

workspacePrompt =
  let generate :: String -> X [(String, [(String, X ())])]
      generate s = do ws <- gets windowset
                      let hid = map W.tag $ W.hidden ws
                          vis = map (W.tag . W.workspace) $ W.visible ws
                          cur = W.tag $ W.workspace $ W.current ws
                          tags = cur:(hid++vis)
                          existing = map (actions (length tags)) $ filter (isInfixOf s) tags

                          lbl t
                            | t == cur = t
                            | t `elem` vis = t
                            | otherwise = t++"*"

                          existing' = map (\(l, a) -> (lbl l, a)) existing

                          new = (s, [("create", addWorkspace s)
                                    ,("ren", renameWorkspaceByName s)
                                    ,("shift", withFocused $
                                       \w -> do addHiddenWorkspace s
                                                windows $ W.shiftWin s w)])
                      return $ if (null s) || (s `elem` tags) then existing'
                               else existing' ++ [new]

      actions c t = (t, [ ("gview", windows $ W.greedyView t)
                      , ("view", windows $ W.view t)
                      , ("shift", windows $ W.shift t) ]
                      ++
                      if c <= 2 -- t `elem` fixedWorkspaces
                      then []
                      else [("del", (windows $ W.view t) >> killAll >> removeWorkspace)])

  in
    select myConfig {prompt = "ws: ", keymap = ("M-w", promptNextOption):(keymap myConfig)} generate

promptKeys = [ ("M-p", runPrompt)
             , ("M-e", updateWindowRing >> windowPrompt)
             , ("M-w", workspacePrompt)
             ]

rotWindow dir = do w <- Local.Ring.rotate dir :: X (Maybe Window)
                   whenJust w $ (windows . W.focusWindow)

promptsLogHook = updateWindowRing

updateWindowRing = (Local.Ring.update $ fmap (liftM2 (,) W.peek W.allWindows) (gets windowset))
