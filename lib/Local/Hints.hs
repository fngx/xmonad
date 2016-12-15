module Local.Hints () where

import Data.List
import qualified Data.Map.Strict as M
import XMonad hiding (config)
import qualified XMonad (config)
import XMonad.Util.Font
import XMonad.Util.Types
import XMonad.Util.XUtils
import Local.Prompt (readKey, nextKeyEvent)

-- an alternative to normal submaps which displays hints as you type

type KeyBinding = (KeyMask, KeySym)

toKeySequence :: String -> [String]
toKeySequence = words

toKeyBinding :: String -> KeyBinding
toKeyBinding = undefined

toBindingSequence :: String -> [KeyBinding]
toBindingSequence s = map toKeyBinding $ toKeySequence s

hintedKeysP :: conf -> [(String, (String, X ()))] -> conf
hintedKeysP conf ks =
  let ks' = map (\(s, a) -> toBindingSequence s, a) ks -- does this need an X
     -- now we make a map from first key to chains of subsequent keys and actions
      byFirstKey = M.fromListWith (++) [ (head k), [(tail k, v)] | (k, v) <- bks ]
     -- now we want to produce bindings for each of the first keys
  in conf

-- given a series of keys we already pressed, and a bunch of things we could press and what they do
-- handle keys until we get there
hintedKeyMap :: [KeyBinding] -> [([KeyBinding], (String, X ()))] -> X ()
hintedKeyMap pfx acs = do
  -- make a window
  XConf {display = d, theRoot = rw, XMonad.config} <- ask
  (Rectangle sx sy sw sh) <- gets $ screenRect . W.screenDetail . W.current . windowset

  font <- initXMF $ "xft:Monospace-10"
  extent <- textExtentsXMF font "ASDKFH"

  let wh = (fi $ fst extent) + (fi $ snd extent) + 2

  win <- io $ mkUnmanagedWindow d (defaultScreenOfDisplay d) rw sx (fi $ sy + (fi sh) - (fi wh)) sw (fi wh)
  gc <- io $ createGC d win

  io $ mapWindow d win

  -- event mask for window
  io $ selectInput d win $ exposureMask .|. keyPressMask

  let hintedKeyMap' :: [KeyBinding] -> [([KeyBinding], (String, X()))] -> X ()
      hintedKeyMap' prefix actions' =
        let actions = filter (\(p, _) -> pfx `isPrefixOf` p) actions'
            prefixs = concatMap printBinding prefix
        in
        do paintWindow win sw wh 1 "#444" "#888"
           -- write the prefix we have typed
           printStringXMF d win font gc "white" "" 1 1 prefixs
           -- write next keys you could press

           -- read the next key and act on it
           keym <- nextKeyEvent d
           -- think about key
           return ()

  status <- io $ grabKeyboard d w True grabModeAsync grabModeAsync currentTime
  when (status == grabSuccess) $ do
    hintedKeyMap' pfx acs
    io $ ungrabKeyboard d currentTime

  io $ sync d False

  releaseXMF font
  io $ freeGC d gc
  io $ destroyWindow d win

hintedKeyMap' :: XMonadFont -> Window -> [KeyBinding] -> ([KeyBinding], (String, X())) -> X()
hintedKeyMap' font win pfx acs' =
  let acs' = filter (\(p, _) -> pfx `isPrefixOf` p) acs' in
    do fillRectangle dpy
