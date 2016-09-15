module XMonad.Prompt.WindowPrompt2 where

import qualified Data.Map as M

import qualified XMonad.StackSet as W
import XMonad
import XMonad.Prompt
import XMonad.Actions.CopyWindow
import XMonad.Actions.WindowBringer
import Data.Maybe

data WindowPrompt = WindowPrompt String

instance XPrompt WindowPrompt where
  showXPrompt (WindowPrompt s) = s
  commandToComplete _ x = x
  nextCompletion _ = getNextCompletion

windowPrompt :: WindowPrompt -> XPConfig -> X (Maybe Window)
windowPrompt t c = do
  wm <- windowMap :: X (M.Map String Window)

  mkXPromptWithReturn t c (compList wm) (\s -> return $ fromJust $ M.lookup s wm)
  where
    compList m s = return . filter (searchPredicate c s) . map fst . M.toList $ m
