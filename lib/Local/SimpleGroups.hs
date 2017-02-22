{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, Rank2Types, TypeSynonymInstances, TypeFamilies
  , EmptyDataDecls #-}

module Local.SimpleGroups ( group, groupl, GroupMessage (..), Group )
where

import XMonad hiding (focus)
import qualified XMonad.StackSet as W
import XMonad.Util.Stack
import Control.Applicative ((<$>), (<|>), (<*>))
import Control.Monad (forM)
import Data.List (unfoldr, intercalate, partition)
import Data.Either
import Data.Maybe (fromMaybe, fromJust, isNothing, isJust)
import Control.Arrow (second, first, (&&&), (***))
import Data.IORef

type Gid = Int

data Group l a = G { groupLayout :: l a
                   , lastSt :: Zipper a
                   , capacity :: Int
                   , gid :: Gid
                   }
               deriving (Read, Show)

instance Eq (Group l a) where
  g1 == g2 = (gid g1) == (gid g2)

instance Ord (Group l a) where
  g1 `compare` g2 = (gid g1) `compare` (gid g2)

data Groups lo li a = GS { groups :: Zipper (Group li a)
                         , innerLayout :: li a
                         , outerLayout :: lo (Group li a)
                         , nextGid :: Gid }

deriving instance (Show a, Show (li a), Show (lo (Group li a))) => Show (Groups lo li a)
deriving instance (Read a, Read (li a), Read (lo (Group li a))) => Read (Groups lo li a)

data GroupMessage = ToOuter SomeMessage
                  | ToCurrent SomeMessage
                  | ToAll SomeMessage
                  | ToInner SomeMessage
                  | GetCapacities (IORef (Zipper Int))
                  | ChangeCapacity (Int -> Int) -- of the focused column
                  | AddGroup
                  | DeleteGroup
                  deriving Typeable

instance Message GroupMessage

-- the layout function for public consumption
group :: lo (Group li Window) -> li Window -> [Int] -> Groups lo li Window
group lo li szs = foldl addGroup emptyG $ reverse szs
  where emptyG = (GS { groups = emptyZ , innerLayout = li , outerLayout = lo, nextGid = 0 })

groupl :: lo (Group li Window) -> li Window -> [(Int, li Window)] -> Groups lo li Window
groupl lo li initial = GS { groups = fromIndex (map toGroup (zip [0..] initial)) 0
                          , innerLayout = li, outerLayout = lo
                          , nextGid = length initial }
  where toGroup (g, (c, st)) = (G { groupLayout = st, lastSt = emptyZ, gid = g, capacity = c})

-- what is called by ChangeCapacities

changeCapacity :: (Zipper Int -> X [Int]) -> (Groups li lo Window) -> X (Groups li lo Window)
changeCapacity f g = do
  let currentC = mapZ_ capacity $ groups g
  newC <- f currentC
  -- if this sends an event, then it all goes wrong, a state update gets lost
  -- I think this means we can't do this in this way
  -- we can instead send multiple increment / decrement messages
  -- but not any set messages
  let (curGroups, focused) = toIndex $ groups g

      extendGroups = curGroups ++ (map emptyG [nextGid g ..])
      emptyG n = G { groupLayout = (innerLayout g), lastSt = emptyZ, capacity = 1, gid = n }

      capGroups = zip extendGroups newC

      updateCaps = flip map capGroups $ \(g, c) -> g {capacity = c}
      newGroups  = filter ((> 0) . capacity) updateCaps
      groups' = fromIndex newGroups (fromMaybe 0 focused)

  return $ g { groups = groups', nextGid = 1 + (maximum $ map gid newGroups) }

lengthZ Nothing = 0
lengthZ (Just (W.Stack f u d)) = 1 + length u + length d

anyZ _ Nothing = False
anyZ t (Just (W.Stack f u d)) = t f || any t u || any t d

addGroup g@(GS { groups = gs, innerLayout = il }) c =
  g { groups = insertUpZ newGroup gs , nextGid = (nextGid g + 1)}
  where newGroup =  G { groupLayout = il, lastSt = emptyZ, capacity = c, gid = nextGid g }

both :: (a -> b) -> Either a a -> b
both f = either f f

unTag :: Either a a -> a
unTag = both id

instance (LayoutClass li Window, LayoutClass lo (Group li Window))
  => LayoutClass (Groups lo li) Window where

  description (GS { outerLayout = o, groups = g }) =
    "[" ++ (concat $ W.integrate' $ mapZ_ (description . groupLayout) g) ++ "]"

  -- layouting
  runLayout wspa@(W.Workspace _ g0 st) r = do
    let ws = W.integrate' st
        theFocus = W.focus <$> st

        -- make sure there is a group
        g = if null (groups g0) then addGroup g0 1 else g0

        -- if there is a group with capacity Nothing, it is the overflow
        -- otherwise the overflow is the rightmost group
        -- if there are two groups with capacity nothing, not really sure
        -- could fill them all up evenly?

        -- it would be nice to adjust the capacity when windows appear or are deleted
        allocate :: [Group li Window] -> [Window] -> [(Group li Window, [Window])]
        allocate [] _ = []
        allocate (g:[]) ws = [(g, ws)]
        allocate gs [] = map (flip (,) []) gs
        allocate (g:gs) ws = let (wl, wr) = splitAt (capacity g) ws in (g, wl):(allocate gs wr)

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

  -- message handling
  handleMessage l sm
    | Just e@(ButtonEvent {}) <- fromMessage sm = bcast e
    | Just e@(PropertyEvent {}) <- fromMessage sm = bcast e
    | Just e@(ExposeEvent {}) <- fromMessage sm = bcast e
    where
      bcast m = handleMessage l $ SomeMessage $ ToAll $ SomeMessage m

  handleMessage l sm
    | Just m@Hide <- fromMessage sm = bcast m
    | Just m@ReleaseResources <- fromMessage sm = bcast m
    where
      bcast m = handleMessage l $ SomeMessage $ ToAll $ SomeMessage m

  handleMessage g sm = case fromMessage sm of
    Just (ToAll sm') -> do gm <- handleOuter g sm'
                           im <- handleInner (fromMaybe g gm) sm'
                           return $ im <|> gm
    Just (ToOuter sm') -> handleOuter g sm'
    Just (ToCurrent sm') -> handleCurrent g sm'
    Just (ToInner sm') -> handleInner g sm'
    Just (GetCapacities r) -> let cs = mapZ_ capacity $ groups g in
                                do io (modifyIORef' r (const cs))
                                   return Nothing
    Just (ChangeCapacity f) -> return $ Just g { groups = flip onFocusedZ (groups g) $
                                                 \g -> g { capacity = f (capacity g)} }

    Just (AddGroup) -> return $ Just $ addGroup g 1
    Just (DeleteGroup) -> do
      whenJust (getFocusZ $ groups g) $ \f -> do handleMessage (groupLayout f) (SomeMessage Hide)
                                                 return ()
      return $ Just g { groups = deleteFocusedZ $ groups g }
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
