{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, GADTs, BangPatterns #-}

module Local.Ring where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified Data.Sequence as S
import Data.Sequence ((><), (<|), (|>))
import qualified Data.Set as Set
import Control.Monad
import Data.Foldable (toList)

import qualified Debug.Trace as D

-- it is like a zipper but where the focus is optional
data Ring a = Ring
  { focus :: Maybe a
  , left :: S.Seq a
  , right :: S.Seq a
  } deriving (Read, Show, Typeable)

data RingDirection = Prev | Next

instance (Read a, Show a, Typeable a) => ExtensionClass (Ring a) where
  initialValue = Ring Nothing S.empty S.empty
  extensionType = PersistentExtension

rotate :: forall a. (Read a, Show a, Ord a, Typeable a) => RingDirection -> X (Maybe a)
rotate r = do
  XS.modify $ case r of
    Next -> rotateRight
    Prev -> rotateLeft
  (Ring fm _ _) <- XS.get :: X (Ring a)
  return fm
  where
    rotateLeft :: Ring a -> Ring a
    rotateLeft (Ring f l r)
      | S.null l = Ring (toMaybe tr) hr (sFromMaybe f)
      | otherwise = Ring (toMaybe tl) hl (sFromMaybe f >< r)
      where
        !(hr, tr) = S.splitAt (S.length r - 1) r
        !(hl, tl) = S.splitAt (S.length l - 1) l

    rotateRight :: Ring a -> Ring a
    rotateRight (Ring f l r)
      | S.null r = Ring (toMaybe ll) (sFromMaybe f) rl
      | otherwise = Ring (toMaybe lr) (l >< (sFromMaybe f)) rr
      where
        !(ll, rl) = S.splitAt 1 l
        !(lr, rr) = S.splitAt 1 r

update :: (Read a, Show a, Ord a, Typeable a) => X ((Maybe a), [a]) -> X ()
update g = do
  (focus, cands) <- g
  let del = maybe id Set.delete focus
      cands' = del $ Set.fromList cands
      exists = flip Set.member cands'
      filt = S.filter exists
      -- we have candidates without focus in them
      update' (Ring f l r) = result
        where !result = if f == focus then rotatedRing else filteredRing
              !filteredRing = (Ring focus S.empty (f' >< l' >< r'))
              !rotatedRing = (Ring focus l' r')
              !l' = filt l
              !r' = filt r
              !f' = filt $ sFromMaybe f
  XS.modify $ update'

contents :: forall a. (Read a, Show a, Ord a, Typeable a) => X (Maybe a, [a])
contents = do
  (Ring f l r) <- XS.get :: X (Ring a)
  return $ (f, toList $ l >< r)

sFromMaybe :: Maybe a -> S.Seq a
sFromMaybe Nothing = S.empty
sFromMaybe (Just x) = S.singleton x

toMaybe :: S.Seq a -> Maybe a
toMaybe s = if S.null s then Nothing else Just $ S.index s 0
