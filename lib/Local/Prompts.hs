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
import Data.Maybe (fromMaybe, isJust)
import Local.Util
import qualified Data.Map.Strict as M
import System.Directory (getHomeDirectory)
import System.FilePath (takeExtension, dropExtension, combine)

-- uses my prompt utility to provide a few prompts

myConfig = Config
           { normal = ("white", "#222")
           , item = (Theme.normalText, "#333")
           , highlight = ("white", "#666")
           , border = (1, "#777")
           , font = Theme.bigFont
           , prompt = ":"
           , top = False
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
                           ("pop", io $ spawn $ "notify-send \"" ++ c ++ "\" \"$(" ++ c ++ ")\"")
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
                                                  [ ("focus", windows $ W.focusWindow w)
                                                  , ("view", windows $ Windows.greedyFocusWindow w)
                                                  , ("bring", windows $ bringWindow w)
                                                  , ("shift", shiftPrompt "M-s" w)] )

      generate cm s = do named <- Windows.recentWindows >>= mapM (\x -> do n <- getName x
                                                                           t <- fmap (W.findTag x) $ gets windowset
                                                                           return (n, fromMaybe "?" t))
                         return $ map (\(n,c,a) -> (trim 45 n,c,a)) $ filter ((isInfixOf s) . (map toLower) . cName) $ map (actions cm) named
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
                      let (hf, he) = partition (isJust . W.stack) $ W.hidden ws
                          hide = map W.tag he
                          hidf = map W.tag hf

                          vis = map (W.tag . W.workspace) $ W.visible ws
                          cur = W.tag $ W.workspace $ W.current ws
                          tags = cur:(vis ++ hidf ++ hide)
                          existing = map (actions (length tags)) $ filter (isInfixOf s) tags

                          colr t
                            | t == cur = ""
                            | t `elem` vis = "#00ff00"
                            | t `elem` hide = "grey"
                            | otherwise = "#ffff00"

                          existing' = map (\(l, _, a) -> (l, colr l, a)) existing

                          new = (s, "", [("create", addWorkspace s)
                                        ,("ren", renameWorkspaceByName s)
                                        ,("shift", withFocused $ \w -> shiftWindowToNew s w)])
                      return $ if (null s) || (s `elem` tags) then existing'
                               else existing' ++ [new]

      actions c t = (t, "", [ ("greedy view", windows $ W.greedyView t)
                            , ("view", windows $ W.view t)
                            , ("shift", windows $ W.shift t)
                            , ("del", (windows $ W.view t) >> killAll >> removeWorkspace)])
  in
    select myConfig {prompt = "ws: ", keymap = (key, promptNextOption):(keymap myConfig)} generate


passwordPrompt :: X ()
passwordPrompt =
  do h <- io $ getHomeDirectory
     passwordFiles <- io $ runProcessWithInput "find" [ combine h ".password-store"
                                                   ,  "-type" , "f"
                                                   , "-name", "*.gpg"
                                                   , "-printf", "%P\n"] []
     let unsuffix :: String -> String -> String
         unsuffix s i
           | takeExtension i == s = dropExtension i
           | otherwise = i

         passwords = map (unsuffix ".gpg") $ lines passwordFiles

         actions x =
           [ ("pass", spawn$ "passm -c -p " ++ x)
           , ("user + pass", spawn$ "passm -f user -p -c "++x)
           , ("browse", spawn$ "xdg-open $(passm -f url " ++ x++")") ]

         generate :: String -> X [(String, String, [(String, X ())])]
         generate s = return $ map (\x -> (x, "", actions x)) $ filter (isInfixOf s) passwords

     select myConfig {prompt = "pass: "} generate

promptKeys = [ ("M-x", ("execute", runPrompt "M-x"))
             , ("M-<Space>", ("window", windowPrompt "M-<Space>"))
             , ("M-w", ("workspace", workspacePrompt "M-w"))
             , ("M-S-w", ("shift", withFocused (shiftPrompt "M-w")))
             , ("M-a p", ("passwd", passwordPrompt))
             ]
