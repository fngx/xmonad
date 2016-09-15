{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, GADTs, BangPatterns #-}

module XMonad.Actions.Ring where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Sequence as S
import qualified Data.Set as Set
import Control.Monad

import qualified Debug.Trace as DT
import System.IO (hPutStrLn, stderr)

lg l = io $ hPutStrLn stderr l

(<|?) :: Maybe a -> S.Seq a -> S.Seq a
(<|?) Nothing b = b
(<|?) (Just x) b = x S.<| b

update :: (Read a, Show a, Ord a, Typeable a) => X ((Maybe a), [a]) -> X ()
update g = do
  (cur, cands) <- g
  let del = maybe id Set.delete cur
      cands' = del $ Set.fromList cands
      update' (Ring e h) =
        let !newHist = (S.filter (flip Set.member cands') $ e <|? h) in
          Ring cur newHist
  XS.modify $ update'

remove :: Int -> S.Seq a -> S.Seq a
remove k s = S.take (k - 1) s S.>< S.drop k s

rotate :: forall a. (Read a, Show a, Typeable a) => [KeySym] -> KeySym -> (a -> X ()) -> X ()
rotate modKey nextKey act = do
  (Ring c h) <- XS.get :: X (Ring a)
  XConf {theRoot = root, display = d} <- ask

  let event = allocaXEvent $ \p -> do
        maskEvent d (keyPressMask .|. keyReleaseMask) p
        KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
        s <- keycodeToKeysym d c 0
        return (t, s)

      order :: S.Seq a
      order = c <|? h

      len = S.length order

      select :: Int -> X ()
      select n
        | len < 1 = return ()
        | otherwise =
          let offset = n `mod` len
              item = S.index order offset in
            do act item
               (typ, key) <- io event
               case () of
                 () | typ == keyPress && key == nextKey -> select (n + 1)
                    | typ == keyPress || key `elem` modKey ->
                        XS.put (Ring (Just item) (c <|? (remove offset h)))
                    | otherwise -> select n

  -- annoyingly, grabbing here prevents grabbing elsewhere
  -- which can cause problems when ungrabbing afterwards.
  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  select $ maybe 0 (const 1) c
  io $ ungrabKeyboard d currentTime

data Ring a = Ring (Maybe a) (S.Seq a) deriving (Typeable, Read, Show)

instance (Read a, Show a, Typeable a) => ExtensionClass (Ring a) where
  initialValue = Ring Nothing S.empty
  extensionType = PersistentExtension
