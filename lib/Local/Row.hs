{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

-- a layout which provides a mouse resizable pile of windows
-- punning on mouse resizable tile.

module Local.Row where

import Data.Ratio ((%))

import XMonad
import XMonad.Util.XUtils (deleteWindow, showWindow)
import qualified XMonad.StackSet as W
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import XMonad.Layout.Groups ( GroupsMessage (ToEnclosing) )
import qualified XMonad.Util.ExtensibleState as XS

data Axis = V | H deriving (Read, Show, Eq)

flipax :: Axis -> Axis
flipax H = V
flipax V = H

-- todo: maybe change groups to give access to the IDs since I need Ord rather than Eq
-- or I could stop using Map and use [()] instead.
data OrderLayout a = OrderLayout (Pile Int) deriving (Read, Show)

-- what should I do to fix the silly bugs where windows get lost

instance (Show a) => LayoutClass OrderLayout a where
  description (OrderLayout p) = description p

  runLayout (W.Workspace t (OrderLayout p) ms) screen = do
    let ws = W.integrate' ms
        indices = take (length ws) [0..]
        stack' = fmap (\st -> (iterate W.focusDown' st) !! (length $ maybe [] W.up ms)) (W.differentiate indices)
        inverse = M.fromList $ zip indices ws
        invert (i, r) = (fromJust $ M.lookup i inverse, r)
    (indices, p') <- runLayout (W.Workspace t p stack') screen
    return (map invert indices, fmap OrderLayout p')

  handleMessage (OrderLayout p) msg =
    do p' <- handleMessage p msg
       return $ fmap OrderLayout p'

data Pile a = Pile
  {
    axis :: !Axis
  , gap :: !Int
  , sizes :: !(M.Map a Rational)
  , handles :: !(M.Map Window (Handle a))
  , isOuterLayout :: Bool
  , lastFocus :: !(Maybe a)
  } deriving (Read, Show)

data Msg a = Drag (Handle a) Position Rational |
             Grow |
             Shrink |
             Equalize |
             Flip |
             SetAxis Axis
  deriving (Read, Show, Typeable)

instance (Typeable a) => Message (Msg a)

data Handle a = Handle
  {
    between :: !(a,a)
  , position :: !Position
  , range :: !Dimension
  } deriving (Read, Show)

-- for whatever reason, sometimes we miss a Hide message
-- to prevent handle windows getting orphaned, we keep track of all of them
-- and destroy the lot every now and then.
data HandleWindows = HW (S.Set Window) deriving (Typeable, Read, Show)

instance ExtensionClass HandleWindows where
  initialValue = HW $ S.empty

rememberHandle :: Window -> X ()
rememberHandle w = XS.modify (\(HW s) -> HW $ S.insert w s)

forgetHandle :: Window -> X ()
forgetHandle w = do XS.modify (\(HW s) -> HW $ S.delete w s)
                    deleteWindow w

destroyAllHandles :: X ()
destroyAllHandles = do
  (HW s) <- XS.get
  mapM_ deleteWindow s
  XS.put $ HW (S.empty :: S.Set Window)

minSize = 0.05

instance (Typeable a, Show a, Ord a) => LayoutClass Pile a where
  description p = case axis p of
                    H -> "|"
                    V -> "-"
  runLayout (W.Workspace _ state ms) screen = do
    deleteHandles state
    (rects, statem) <- maybe (emptyLayout state screen) (doLayout state screen) ms
    let state' = (fromMaybe state statem)
    newHandles <- createHandles state' screen rects
    return (rects, Just $ state' { handles = newHandles })

  doLayout state screen stack = do
    let (rects, state') = render state screen $ W.integrate stack
    return (rects, Just $ state' { lastFocus = Just $ W.focus stack })

  handleMessage st msg
    | (Just m@(Drag h p origLeftSize)) <- fromMessage msg =
        let (left, right) = between h
            szs = sizes st
        in return $ do currentLeftSize <- M.lookup left szs
                       currentRightSize <- M.lookup right szs
                       let deltaOrigSize = (fromIntegral $ p - position h) % (fromIntegral $ range h)
                           deltaCurrentSize = (origLeftSize + deltaOrigSize) - currentLeftSize
                           safeDelta = min (currentRightSize - minSize) $ max (minSize - currentLeftSize) deltaCurrentSize
                           newLeftSize = currentLeftSize + safeDelta
                           newRightSize = currentRightSize - safeDelta
                       return $ st { sizes = M.insert left newLeftSize $ M.insert right newRightSize $ szs }
    | (Just Hide) <- fromMessage msg = cleanup
    | (Just ReleaseResources) <- fromMessage msg = cleanup
    | (Just Equalize) <- fromMessage msg :: Maybe (Msg a) = return $ Just $ equalize st
    | (Just (Local.Row.Grow)) <- fromMessage msg :: Maybe (Msg a) =
        return $ maybe (Nothing) (\w -> Just $ resz st minSize w) (lastFocus st)
    | (Just (Local.Row.Shrink)) <- fromMessage msg :: Maybe (Msg a) =
        return $ maybe (Nothing) (\w -> Just $ resz st (- minSize) w) (lastFocus st)
    | (Just Flip) <- fromMessage msg :: Maybe (Msg a) = return $ Just $ st { axis = flipax $ axis st }
    | (Just (SetAxis a)) <- fromMessage msg :: Maybe (Msg a) = return $ Just $ st { axis = a }
    | (Just e) <- fromMessage msg :: Maybe Event = resize e (handles st) >> return Nothing
    | otherwise = return Nothing
    where equalize st = let sz = sizes st
                            e :: Rational
                            e = 1%(fromIntegral $ M.size sz)
                        in st {sizes=fmap (const e) sz}

          resz st d w = let sz = sizes st
                            shrink = d/(fromIntegral $ M.size sz)
                        in st {sizes=M.adjust (+ d) w $ fmap (flip (-) shrink) sz}

          maximize st w = let sz = sizes st
                              u = (fromIntegral $ M.size sz) * minSize
                              m = 1 - u
                          in
                            if (M.member w sz) && (m >= minSize) then
                             Just $ st { sizes =
                                         M.insert w m $
                                         fmap (const minSize) sz }
                              else Nothing

          cleanup = destroyAllHandles >> (return $ Just $ st {handles=M.empty})
          ax x y = if (axis st) == H then x else y

          resize e@(ButtonEvent {ev_window = w, ev_event_type = t}) handles =
            if t == buttonPress then
              -- if I am an inner layout and I get a button event
              -- it might be for the outer layout if it is
              maybe (if (isOuterLayout st) then (return ()) else (sendOut e))
              dragHandler $ M.lookup w handles
            else return ()
          resize _ _ = return ()

          sendOut x = sendMessage $ ToEnclosing $ SomeMessage x

          send = if (isOuterLayout st)
                 then sendOut
                 else sendMessage

          dragHandler :: Handle a -> X ()
          dragHandler h = maybe (return ())
            (\r -> (flip mouseDrag (return ()) $
                    \x y -> send $ Drag h ((ax x y) + 3) r))
            (M.lookup (fst (between h)) (sizes st))

deleteHandles :: Pile a -> X ()
deleteHandles (Pile {handles = h}) = do
  mapM_ forgetHandle $ M.keys h

createHandles :: Pile a -> Rectangle -> [(a, Rectangle)] -> X (M.Map Window (Handle a))
createHandles st (Rectangle sx sy sw sh) rects =
  let a = axis st
      g = 6 + (fromIntegral $ gap st)

      glyph = if a == V then xC_sb_v_double_arrow else xC_sb_h_double_arrow
      positionFrom (Rectangle x y w h) = if a == V then (fromIntegral g+y+fromIntegral h) else (fromIntegral g+x+fromIntegral w)
      range = if a == V then sh else sw

      handleRect :: Rectangle -> Rectangle
      handleRect (Rectangle x y w h)
        | a == V    = (Rectangle sx ((y + fromIntegral h) - 6) sw (2*g))
        | otherwise = (Rectangle ((x + fromIntegral w) - 6) sy (2*g) sh)

      createHandleWindow :: Rectangle -> X Window
      createHandleWindow (Rectangle x y w h) = withDisplay $ \d -> do
        rw <- asks theRoot
        let screen = defaultScreenOfDisplay d
            visual = defaultVisualOfScreen screen
            attrmask = cWOverrideRedirect
        win <- io $ allocaSetWindowAttributes $ \a -> do
          set_override_redirect a True
          createWindow d rw x y w h 0 0 inputOnly visual attrmask a
        io $ selectInput d win (exposureMask .|. buttonPressMask)
        cursor <- io $ createFontCursor d glyph
        io $ defineCursor d win cursor
        io $ freeCursor d cursor
        showWindow win
        return win

      createHandle :: ((a, Rectangle), a) -> X (Window, Handle a)
      createHandle ((left, rect), right) = do
        window <- createHandleWindow $ handleRect rect
        rememberHandle window
        return (window, Handle { between = (left, right),
                                 position = positionFrom rect,
                                 range = range })
  in
    fmap M.fromList $ mapM createHandle $ zip rects $ map fst $ drop 1 rects

render :: (Show a, Ord a) => Pile a -> Rectangle -> [a] -> ([(a, Rectangle)], Pile a)
render st screen as =
  let -- lookup sizes
      szs :: [Rational]
      szs = map (flip (M.findWithDefault (1 % (max 1 $ fromIntegral $ M.size $ sizes st))) (sizes st)) as
      t = sum szs
      nszs = map (flip (/) t) szs
      parts = scanl (+) 0 nszs
      -- these tuples go like (0, 0.5), (0.5, 1) or similar
      bounds = zip parts (drop 1 parts)
      -- now we need to cut up the screen with the tuples
      (Rectangle sx sy sw sh) = screen
      cut (l, r)
        | (axis st) == H = (Rectangle (pos sx sw) sy          (ext sw) sh      )
        | otherwise      = (Rectangle sx          (pos sy sh) sw       (ext sh))
        where g = if l == 0 then 0 else (fromIntegral $ gap st)
              pos p h = (floor ((fromIntegral p) + (fromIntegral h) * l) + (fromIntegral g) :: Position)
              ext h = (floor ((fromIntegral h) * (r - l)) - g :: Dimension)

      rects = map cut bounds
  in (zip as rects, st {sizes = M.fromList (zip as nszs)})

row a = Pile { gap = 0
              , sizes = M.empty
              , handles = M.empty
              , axis = a
              , isOuterLayout = False
              , lastFocus = Nothing
              }

orderRow a = OrderLayout ((row a) {isOuterLayout = True})
