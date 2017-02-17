{-# LANGUAGE StandaloneDeriving, FlexibleContexts, DeriveDataTypeable
  , UndecidableInstances, FlexibleInstances, MultiParamTypeClasses
, PatternGuards, Rank2Types, TypeSynonymInstances #-}

module Local.SimpleGroups ( group, GroupMessage (..), Group )
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

data Group l a = G { groupLayout :: l a
                   , lastSt :: Zipper a
                   , capacity :: Maybe Int
                   , gid :: Int
                   }
               deriving (Read, Show)

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
                  -- | Rearrange (Arrangement -> Arrangement)
                  | ChangeCapacities (Zipper (Maybe Int) -> [Maybe Int])
                  deriving Typeable

instance Message GroupMessage

-- the layout function for public consumption
group :: lo (Group li Window) -> li Window -> [Maybe Int] -> Groups lo li Window
group lo li szs = foldl addGroup emptyG $ reverse szs
  where emptyG = (GS { groups = emptyZ , innerLayout = li , outerLayout = lo})

-- what is called by ChangeCapacities

changeCapacity :: (Zipper (Maybe Int) -> [Maybe Int]) -> (Groups li lo Window) -> (Groups li lo Window)
changeCapacity f g =
  let currentC = mapZ_ capacity $ groups g
      newC = f currentC

      (curGroups, focused) = toIndex $ groups g
      extendGroups = curGroups ++ (map emptyG [length curGroups..])
      emptyG n = G { groupLayout = (innerLayout g), lastSt = emptyZ, capacity = Nothing, gid = n}

      capGroups = zip extendGroups newC

      updateCaps = flip map capGroups $ \(g, c) -> g {capacity = c}
      newGroups  = filter ((maybe True (> 0)) . capacity) updateCaps
      groups' = fromIndex newGroups (fromMaybe 0 focused)
  in g { groups = groups' }

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
    description o ++ "[" ++ (intercalate "," $ W.integrate' $ mapZ_ label g) ++ "]"
    where label g = maybe "*" show (capacity g) ++ description (groupLayout g)

  -- layouting
  runLayout wspa@(W.Workspace _ g0 st) r = do
    let ws = W.integrate' st
        theFocus = W.focus <$> st

        -- make sure there is a group
        g = if null (groups g0) then addGroup g0 Nothing else g0

        -- if there is a group with capacity Nothing, it is the overflow
        -- otherwise the overflow is the rightmost group
        -- if there are two groups with capacity nothing, not really sure
        -- could fill them all up evenly?
        allocate :: [Group li Window] -> [Window] -> [(Group li Window, [Window])]
        allocate [] _ = []
        allocate (g:[]) ws = [(g, ws)]
        allocate gs [] = map (flip (,) []) gs
        allocate (g:gs) ws = case capacity g of
          Nothing -> let capR = sum $ map (fromMaybe 0 . capacity) gs
                         nthR :: Int
                         nthR = 1 + (length $ filter (isNothing . capacity) gs)
                         nws = length ws
                         excess = nws - capR
                         (wsHere, wsLeft) = splitAt (excess `div` nthR) ws
                     in (g, wsHere):(allocate gs wsLeft)
          Just c -> let (wl, wr) = splitAt c ws in (g, wl):(allocate gs wr)

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
    -- Just (Rearrange f) -> arrange f g
    Just (ChangeCapacities f) -> return $ Just $ changeCapacity f g

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





-- -- analogous to windows for the normal stack
-- -- takes an updater for the double-stack, and updates the normal stack
-- -- plus the grouping rules. the real stuff is in arrange below
-- rearrange :: (Arrangement -> Arrangement) -> X ()
-- rearrange f = sendMessage $ Rearrange f

-- createColumn = rearrange $
--   \arr -> let arr' :: Arrangement
--               arr'  = onFocusedZ (second deleteFocusedZ) arr
--               focus = do focusGroup <- getFocusZ arr
--                          focusWindow <- getFocusZ $ snd focusGroup
--                          return focusWindow
--           in maybe arr (\w -> insertUpZ (Nothing, singletonZ w) arr') focus

-- not public
-- arrange :: (Arrangement -> Arrangement) -> Groups li lo Window -> X (Maybe (Groups li lo Window))
-- arrange f g =
--   let currentA = mapZ_ ((Just . gid) &&& lastSt) $ groups g
--       newA = f currentA

--       -- each thing in the arrangement is a group which has a certain number of windows
--       -- figuring out when to delete groups requires a little work
--       --  any empty group preceding a nonempty group should be deleted
--       --  any nonempty group which had a capacity limit should have its limit
--       --  set to its current size
--       --  new groups get their current size and the default layout

--       groupIndex = map (gid &&& id) $ W.integrate' $ groups g

--       lr True = Right
--       lr False = Left

--       updateGroup isF (nid, rest) (Nothing, stk) =
--         (nid+1, (lr isF $ G { gid = nid, lastSt = stk, capacity = Just $ lengthZ stk, groupLayout = innerLayout g }) : rest)
--       updateGroup isF (nid, rest) (Just gid, stk) =
--           case lookup gid groupIndex of
--             Just oldG -> (nid, (lr isF $ oldG { lastSt = stk
--                                              , capacity = (const $ lengthZ stk) <$> (capacity oldG)
--                                              }) : rest)
--             Nothing -> updateGroup isF (nid, rest) (Nothing, stk)

--       -- TODO: append leftover groups
--       -- TODO: delete dead groups
--       newG = g { groups = let (_, groups') = foldlZ updateGroup (lengthZ $ groups g, []) newA
--                           in fromTags groups' }

--       -- stuff to flatten an arrangement back into a stack of windows
--       flatten1 isF (_, subStack) = W.integrate' $ flip mapZ subStack (flatten2 isF)
--       flatten2 True True = Right
--       flatten2 _ _ = Left

--       newStack = fromTags $ concat $ W.integrate' $ flip mapZ newA flatten1
--   in if newA == currentA then return Nothing
--      else do windows $ W.modify Nothing $ const newStack
--              return $ Just newG -- not sure how this will work out, but bam, let's just do it



-- type Arrangement = Zipper (Maybe Int, Zipper Window)
