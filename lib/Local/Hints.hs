module Local.Hints () where

import qualified Data.Map.Strict as M

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

hintedKeyMap :: String -> ([KeyBinding], String, X ()) -> X ()
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

  -- a loop where we read keys and look for appropriate subsets of the input string

  releaseXMF font
  io $ freeGC d gc
  io $ destroyWindow d win
