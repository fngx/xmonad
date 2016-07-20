{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, GADTs #-}

module XMonad.Actions.Ring where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Sequence as S
import qualified Data.Set as Set

import qualified Debug.Trace as DT
import System.IO (hPutStrLn, stderr)

lg l = io $ hPutStrLn stderr l

(<|?) :: Maybe a -> S.Seq a -> S.Seq a
(<|?) Nothing b = b
(<|?) (Just x) b = x S.<| b

update :: (Show a, Ord a, Typeable a) => X ((Maybe a), [a]) -> X ()
update g = do
  (cur, cands) <- g
  let del = maybe id Set.delete cur
      cands' = del $ Set.fromList cands
      update' (Ring e h) =
        Ring cur (S.filter (flip Set.member cands') $ e <|? h)
  XS.modify $ update'

remove :: Int -> S.Seq a -> S.Seq a
remove k s = S.take (k - 1) s S.>< S.drop k s

rotate :: forall a. (Show a, Typeable a) => [KeySym] -> KeySym -> (a -> X ()) -> X ()
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
        | len < 2 = return ()
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

  io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
  select 1
  io $ ungrabKeyboard d currentTime

data Ring a = Ring (Maybe a) (S.Seq a) deriving (Show)

instance (Show a, Typeable a) => ExtensionClass (Ring a) where
  initialValue = Ring Nothing S.empty
