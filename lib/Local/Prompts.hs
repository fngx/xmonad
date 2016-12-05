module Local.Prompts (promptKeys) where

import qualified Local.Theme as Theme
import qualified Local.Windows as Windows
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
import Data.Maybe (fromMaybe)
import Local.Util
import qualified Data.Map.Strict as M

-- uses my prompt utility to provide a few prompts

myConfig = Config
           { normal = ("white", "#222")
           , item = (Theme.normalText, "#333")
           , highlight = ("white", "#666")
           , border = (2, "#777")
           , font = "xft:Sans-13"
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
                      , ("C-w", promptClear)
                      ]
                      ++
                      [(k, promptNextOption) | k <- ["C-n", "<Tab>", "C-i", "<Right>"]]
                      ++
                      [(k, promptPrevOption) | k <- ["C-p", "S-<Tab>", "C-S-I", "<Left>"]]
           , rawKeymap = [ ((1, 65056), promptPrevOption)] -- shift tab?
           }

runPrompt key =
  let actions c = (c, "", [("run", io $ spawn c),
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

shiftWindowToNew ws w = do addHiddenWorkspace ws
                           windows $ W.shiftWin ws w

-- TODO trim window names
-- TODO maybe indicate ws for windows
windowPrompt key =
  let actions :: (M.Map String String) -> (NamedWindow, String) -> (String, String, [(String, X ())])
      actions cm (nw, c) = let w = unName nw in (show nw ++ " [" ++ c ++ "]", M.findWithDefault "" c cm,
                                                  [ ("view", windows $ W.focusWindow w)
                                                  , ("greedy", windows $ Windows.greedyFocusWindow w)
                                                  , ("bring", windows $ bringWindow w)
                                                  , ("shift", shiftPrompt "M-s" w)] )

      generate cm s = do named <- Windows.recentWindows >>= mapM (\x -> do n <- getName x
                                                                           t <- fmap (W.findTag x) $ gets windowset
                                                                           return (n, fromMaybe "?" t))
                         return $ map (\(n,c,a) -> (trim 38 n,c,a)) $ filter ((isInfixOf s) . (map toLower) . cName) $ map (actions cm) named
  in
    do ws <- gets windowset
       let hid = map W.tag $ W.hidden ws
           vis = map (W.tag . W.workspace) $ W.visible ws
           cur = W.tag $ W.workspace $ W.current ws
           tags = cur:(vis++hid)

           colrs = cycle [ "#90ee90"
                         , "#ffa500"
                         , "#ff82ab"
                         , "#bcd2ee"
                         , "#ffff00" ]

           colrMap = M.fromList $ zip (sort tags) colrs

       select myConfig { prompt = "win: "
                       , keymap = (key, promptNextOption):("M-w", promptCycleInput tags):(keymap myConfig)
                       } (generate colrMap)

swap2 (a:(b:cs)) = b:(a:cs)
swap2 x = x

shiftPrompt key w =
  let generate :: String -> X [(String, String, [(String, X ())])]
      generate s = do ws <- gets windowset
                      let hid = map W.tag $ W.hidden ws
                          vis = map (W.tag . W.workspace) $ W.visible ws
                          tags = (vis++hid)
                          existing = map (actions (length tags)) $ filter (isInfixOf s) tags

                          colr t
                            | t `elem` vis = "#00ff00"
                            | otherwise = "#ffff00"

                          existing' = map (\(l, a) -> (l, colr l, a)) existing

                          new = (s, "#00ff00", [ ("shift", shiftWindowToNew s w)])
                      return $ if (null s) || (s `elem` tags) then existing'
                               else existing' ++ [new]

      actions c t = (t, [ ("shift", windows $ W.shiftWin t w) ])

  in do ws <- gets windowset
        let hid = map W.tag $ W.hidden ws
            vis = map (W.tag . W.workspace) $ W.visible ws

        select myConfig {prompt = "shift: ", keymap = (key, promptNextOption):("M-h", promptCycleInput hid):("M-v", promptCycleInput vis):(keymap myConfig)} generate


workspacePrompt key =
  let generate :: String -> X [(String, String, [(String, X ())])]
      generate s = do ws <- gets windowset
                      let hid = map W.tag $ W.hidden ws
                          vis = map (W.tag . W.workspace) $ W.visible ws
                          cur = W.tag $ W.workspace $ W.current ws
                          tags = cur:(vis ++ hid)
                          existing = map (actions (length tags)) $ filter (isInfixOf s) tags

                          colr t
                            | t == cur = ""
                            | t `elem` vis = "#00ff00"
                            | otherwise = "#ffff00"

                          existing' = map (\(l, _, a) -> (l, colr l, a)) existing

                          new = (s, "", [("create", addWorkspace s)
                                        ,("ren", renameWorkspaceByName s)
                                        ,("shift", withFocused $ \w -> shiftWindowToNew s w)])
                      return $ if (null s) || (s `elem` tags) then existing'
                               else existing' ++ [new]

      actions c t = (t, "", [ ("gview", windows $ W.greedyView t)
                            , ("view", windows $ W.view t)
                            , ("shift", windows $ W.shift t)
                            , ("del", (windows $ W.view t) >> killAll >> removeWorkspace)])

  in
    select myConfig {prompt = "ws: ", keymap = (key, promptNextOption):(keymap myConfig)} generate

promptKeys = [ ("M-x", runPrompt "M-x")
             , ("M-<Space>", windowPrompt "M-<Space>")
             , ("M-b", workspacePrompt "M-b")
             , ("M-S-b", withFocused (shiftPrompt "M-b"))
             ]
