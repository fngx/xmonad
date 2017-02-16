{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
, PatternGuards, Rank2Types, TypeSynonymInstances #-}

module Local.SimpleGroups ( group, GroupMessage (..), Group (..), Groups (..) ) where

import XMonad hiding (focus)
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import Control.Applicative ((<$>), (<|>))
import Control.Monad (forM)
import Data.List (unfoldr, intercalate, partition)
import Data.Either
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import Control.Arrow (second, first)

data Group l a = G { groupLayout :: l a
                   , lastSt :: Zipper a
                   , capacity :: Int
                   , gid :: Int
                   }
               deriving (Read, Show)

-- do I want to implement this directly?

instance Eq (Group l a) where
  g1 == g2 = (gid g1) == (gid g2)

instance Ord (Group l a) where
  g1 `compare` g2 = (gid g1) `compare` (gid g2)

data Groups lo li a = GS { groups :: Zipper (Group li a)
                         , innerLayout :: li a
                         , outerLayout :: lo (Group li a)}


deriving instance (Show a, Show (li a), Show (lo (Group li a))) => Show (Groups lo li a)
deriving instance (Read a, Read (li a), Read (lo (Group li a))) => Read (Groups lo li a)

data GroupMessage = ToOuter SomeMessage
                  | ToCurrent SomeMessage
                  | ToAll SomeMessage
                  | ToInner SomeMessage
--                  | Reorganize
                  deriving Typeable

instance Message GroupMessage

lengthZ Nothing = 0
lengthZ (Just (W.Stack f u d)) = 1 + length u + length d

anyZ _ Nothing = False
anyZ t (Just (W.Stack f u d)) = t f || any t u || any t d

addGroup g@(GS { groups = gs, innerLayout = il }) c =
  g { groups = insertUpZ newGroup gs }
  where newGroup =  G { groupLayout = il, lastSt = emptyZ, capacity = c, gid = (lengthZ gs) }

both :: (a -> b) -> Either a a -> b
both f = either f f

unTag :: Either a a -> a
unTag = both id

instance (LayoutClass li Window, LayoutClass lo (Group li Window))
  => LayoutClass (Groups lo li) Window where

  description (GS { outerLayout = o, groups = g }) =
    description o ++ "[" ++ (intercalate "," $ W.integrate' $ mapZ_ (description . groupLayout) g) ++ "]"

  runLayout wspa@(W.Workspace _ g0 st) r = do
    let ws = W.integrate' st
        theFocus = W.focus <$> st

        -- make sure there is a group
        g = if null (groups g0) then addGroup g0 0 else g0

        -- put the windows with the groups
        allocation = allocate (W.integrate' $ groups g) ws

        reconstruct :: (Group li Window, [Window]) -> Either (Group li Window) (Group li Window)
        reconstruct (group, []) = Left $ group { lastSt = Nothing }
        reconstruct (group, ws) = gtag $ group { lastSt = fromTags tags }
          where tags :: [Either Window Window]
                tags = map (if hasFocus then tag theFocus else tag lastFocus) ws
                lastFocus   = (getFocusZ $ lastSt group)
                hasFocus    = fromMaybe False $ flip elem ws <$> theFocus
                tag f x     = if f == Just x then Right x else Left x
                gtag :: Group li Window -> Either (Group li Window) (Group li Window)
                gtag        = if hasFocus then Right else Left

        groups' = fromTags $ map reconstruct allocation

    -- run outer layout for nonempty groups
    (outerRects, outerLayoutM) <- runLayout wspa { W.layout = (outerLayout g)
                                                 , W.stack  = filterZ_ (isJust . lastSt) groups'
                                                 } r

    -- hide / run inner layouts
    let rectIndex = map (first gid) outerRects

    rectsStates <- flip mapZM_ groups' $ \ig@(G { groupLayout = l, lastSt = st, gid = gi }) -> do
      let rectM = lookup gi rectIndex
      case rectM of
        Nothing -> do l' <- handleMessage l $ SomeMessage Hide
                      return $ ([], ig { groupLayout = fromMaybe l l' })
        Just rect -> do (rs, l') <- runLayout wspa { W.layout = l, W.stack = st } rect
                        return $ (rs, ig { groupLayout = fromMaybe l l' })

    let rects = concatMap fst $ W.integrate' rectsStates
        gstates = mapZ_ snd rectsStates

    -- return the rects and update the state
    return $ (rects, Just $ g { outerLayout = fromMaybe (outerLayout g) outerLayoutM
                              , groups = gstates })

    where
      -- stick the windows to the groups using group capacities
      -- the first 0-capacity group is elastStic
      allocate :: [Group l a] -> [a] -> [(Group l a, [a])]
      allocate [] _ = []
      allocate gs [] = map (flip (,) []) gs
      allocate (g:gs) as
        | capacity g == 0 || null gs =
            let sparecap = sum $ map capacity gs
                n = length as
                (als, ars) = splitAt (n - sparecap) as
            in (g, als):(allocate gs ars)
        | otherwise =
            let (als, ars) = splitAt (capacity g) as
            in (g, als):(allocate gs ars)

  handleMessage l sm
    | Just e@(ButtonEvent {}) <- fromMessage sm = bcast e
    | Just e@(PropertyEvent {}) <- fromMessage sm = bcast e
    | Just e@(ExposeEvent {}) <- fromMessage sm = bcast e
    | Just m@Hide <- fromMessage sm = bcast m
    where
      bcast m = handleMessage l $ SomeMessage $ ToAll $ SomeMessage m

  handleMessage g sm = case fromMessage sm of
    Just (ToAll sm') -> do gm <- handleOuter g sm'
                           im <- handleInner (fromMaybe g gm) sm'
                           return $ im <|> gm
    Just (ToOuter sm') -> handleOuter g sm'
    Just (ToCurrent sm') -> handleCurrent g sm'
    Just (ToInner sm') -> handleInner g sm'
    -- TODO generic 'reorganize' message
    --      it could invoke windows to reset the current stack
    --      and insert new groups at the same time
    -- Just (WithStacks act) -> do act $ flip mapZ_ (groups g) $ \gi -> (capacity gi, lastSt gi)
    --                             return Nothing
    Nothing -> handleMessage g $ SomeMessage $ ToCurrent sm

    where
      handleInner g@(GS { groups = gs }) sm' =
        do updates <- flip mapZM_ gs $
                      \gi -> do gl' <- handleMessage (groupLayout gi) sm'
                                return $ case gl' of
                                           Nothing -> Left gi
                                           Just gl -> Right $ gi {groupLayout = gl}

           -- if there are any rights, we want to take all the rights
           -- otherwise nothing
           if anyZ isRight updates then
             return $ Just $ g { groups = mapZ_ unTag updates }
             else
             return Nothing

      handleOuter g@(GS { groups = gs }) sm' =
        handleMessage (outerLayout g) sm' >>=
        return . fmap (\x -> g { outerLayout = x })

      handleCurrent g@(GS { groups = gs }) sm' =
        let foc = getFocusZ gs
            res = case foc of
                    Nothing -> return Nothing
                    Just (G { groupLayout = l }) ->
                      handleMessage l sm' >>=
                      return . fmap (\x -> g { groups = onFocusedZ (\f -> f { groupLayout = x }) gs })
        in do res' <- res
              case res' of
                Just x -> return res'
                Nothing -> handleOuter g sm'

group :: lo (Group li Window) -> li Window -> [Int] -> Groups lo li Window
group lo li szs = foldl addGroup emptyG $ reverse szs
  where emptyG = (GS { groups = emptyZ , innerLayout = li , outerLayout = lo})
