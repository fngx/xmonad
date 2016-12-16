module Local.Hints (hintedKeysP) where

import Data.Bits
import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import XMonad hiding (config)
import qualified XMonad (config)
import qualified XMonad.StackSet as W
import XMonad.Util.Font
import XMonad.Util.Types
import XMonad.Util.XUtils
import Local.Prompt (readKey, nextKeyEvent, mkUnmanagedWindow)
import Data.Maybe (mapMaybe, fromMaybe, fromJust, isJust)
import Graphics.X11.Xlib.Misc (keysymToString)
import qualified Debug.Trace as D

import Control.Concurrent (threadDelay)

-- an alternative to normal submaps which displays hints as you type

type Key = (KeyMask, KeySym)
data KeyTree = Leaf String (X ()) | Sub (M.Map Key KeyTree)

instance Show KeyTree where
  show (Leaf d _) = d
  show (Sub m) = intercalate " | " $ map show1 $ M.toList m
    where show1 (k, a) = (showKey k) ++": "++(show2 a)
          show2 l@(Leaf _ _) = show l
          show2 (Sub m) = "{" ++ (intercalate ", " $ map (show2 . snd) $ M.toList m) ++ "}"

emptyKT = Sub M.empty

hintedKeysP :: XConfig a -> [(String, (String, X()))] -> XConfig a
hintedKeysP conf ks =
  conf
  { keys = \cnf ->
             let kt = toKeyTree (modMask cnf) ks
                 binds = toBindings kt
             in M.union (M.fromList binds) (keys conf cnf)
  }
  where
    toBindings (Leaf _ _) = []
    toBindings (Sub m) = flip map (M.toList m) $ \(a, b) -> (a, toBindings1 a b)
    toBindings1 _ (Leaf _ a) = a
    toBindings1 p t = runKeyTree p t

toKeyTree :: KeyMask -> [(String, (String, X ()))] -> KeyTree
toKeyTree mask keys = foldl insertKey emptyKT $
  flip map keys $ \(a, (b, c)) -> (mapMaybe (readKey mask) $ words a, Leaf b c)

insertKey :: KeyTree -> ([Key], KeyTree) -> KeyTree
insertKey t (keys, subtree)
  -- if there are no more keys then we just do the action
  -- and who cares about the input because there are no keys!
  | null keys = subtree
  -- if there are keys, we want to update t
  | otherwise =
      let (k1:ks) = keys
          rhs = insertKey emptyKT (ks, subtree)
          merge Nothing = Just rhs
          merge (Just t') = Just $ insertKey t' (ks, subtree)
      in
      case t of
        Leaf _ _ -> rhs
        Sub m -> Sub $ M.alter merge k1 m

-- blaa this is in xlib.misc or something equally stupid
showKey :: Key -> String
showKey (masks, sym) =
  intercalate "-" $ reverse $ (keysymToString sym):
     [modName | (modName, modMask) <-
                [ ("M1", mod1Mask) ,
                  ("M2", mod2Mask) ,
                  ("M3", mod3Mask) ,
                  ("M", mod4Mask) ,
                  ("M5", mod5Mask) ,
                  ("C", controlMask) ,
                  ("S", shiftMask) ]
              , (modMask .&. masks) /= zeroBits ]

runKeyTree :: Key -> KeyTree -> X ()
runKeyTree pfx kt = do
  -- make a window
  XConf {display = d, theRoot = rw} <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  font <- initXMF $ "xft:Monospace-10"
  extent <- textExtentsXMF font "ASDKFH"

  let wh = (fi $ fst extent) + (fi $ snd extent) + 2

  win <- io $ mkUnmanagedWindow d (defaultScreenOfDisplay d) rw sx (fi $ sy + (fi sh) - (fi wh)) sw (fi wh)
  gc <- io $ createGC d win

  io $ mapWindow d win

  -- event mask for window
  io $ selectInput d win $ exposureMask .|. keyPressMask

  let y0 = 1 + fst extent
  let x0 = 2

  let render :: String -> (String, String) -> String -> X ()
      render prefix (message, colr) border = do
        paintWindow win sw wh 1 "#444" border
        printStringXMF d win font gc "white" "#444" x0 (1 + (fst extent)) prefix
        plength <- textWidthXMF d font prefix
        let x1 = fi plength + fi x0
        printStringXMF d win font gc colr "#444444" (8 + fi x1) y0 message
        io $ sync d False

      runKT :: [Key] -> KeyTree -> X ()
      runKT prefix kt = do
        let prefixs = intercalate " " $ map showKey prefix
        case kt of
          Leaf n a -> do render prefixs (n, "green") "#fff"
                         io $ threadDelay 400000
                         a
          Sub m -> do let nexts = show kt
                      render prefixs (nexts, "#999") "#999"
                      keym <- nextKeyEvent d
                      let handle (km, k, s) = maybe (noMatch km k s) (runKT (prefix ++ [(km, k)])) $ M.lookup (km, k) m
                          cont = runKT prefix kt
                          noMatch km k s
                            | km == controlMask && k == xK_g = return ()
                            | km == 0 && k == xK_Escape = return ()
                            | not $ null s = (render "" (prefixs ++ " " ++ showKey (km, k) ++ " is undefined", "#f66") "#fff") >> (io $ threadDelay 800000)
                            | otherwise = cont
                      maybe cont handle keym

  status <- io $ grabKeyboard d win True grabModeAsync grabModeAsync currentTime

  when (status == grabSuccess) $ do
    runKT [pfx] kt
    io $ ungrabKeyboard d currentTime

  io $ sync d False

  releaseXMF font
  io $ freeGC d gc
  io $ destroyWindow d win
