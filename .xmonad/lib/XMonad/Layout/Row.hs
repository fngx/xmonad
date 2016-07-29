{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

-- a layout which provides a mouse resizable pile of windows
-- punning on mouse resizable tile.

module XMonad.Layout.Row where

import Data.Ratio ((%))

import XMonad
import XMonad.Util.XUtils (deleteWindow, showWindow)
import qualified XMonad.StackSet as W
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import XMonad.Layout.Groups ( GroupsMessage (ToEnclosing) )

data Axis = V | H deriving (Read, Show, Eq)

-- todo: maybe change groups to give access to the IDs
data OrderLayout a = OrderLayout (Pile Int) deriving (Read, Show)

instance (Show a) => LayoutClass OrderLayout a where
  description (OrderLayout p) = description p
  doLayout (OrderLayout p) screen stack = do
    let ws = W.integrate stack
        indices = take (length ws) [0..]
        stack' = fromJust $ W.differentiate indices
        inverse = M.fromList $ zip indices ws
        invert (i, r) = (fromJust $ M.lookup i inverse, r)
    (indices, p') <- doLayout p screen stack'
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
  } deriving (Read, Show)

data Msg a = Drag (Handle a) Position Rational |
             Grow a |
             Shrink a |
             Maximize a |
             Equalize
  deriving (Read, Show, Typeable)

instance (Typeable a) => Message (Msg a)

data Handle a = Handle
  {
    between :: !(a,a)
  , position :: !Position
  , range :: !Dimension
  } deriving (Read, Show)

minSize = 0.05

instance (Typeable a, Show a, Ord a) => LayoutClass Pile a where
  description p = show $ axis p

  doLayout state screen stack = do
    deleteHandles state
    let (rects, state') = render state screen $ W.integrate stack
    newHandles <- createHandles state' screen rects
    return (rects, Just $ state' {handles = newHandles})

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
    | (Just (Maximize win)) <- fromMessage msg = return $ maximize st win
    | (Just e) <- fromMessage msg :: Maybe Event = resize e (handles st) >> return Nothing
    | otherwise = return Nothing
    where equalize st = let sz = sizes st
                            e :: Rational
                            e = 1%(fromIntegral $ M.size sz)
                        in st {sizes=fmap (const e) sz}

          maximize st w = let sz = sizes st
                              u = (fromIntegral $ M.size sz) * minSize
                              m = 1 - u
                          in
                            if (M.member w sz) && (m >= minSize) then
                             Just $ st { sizes =
                                         M.insert w m $
                                         fmap (const minSize) sz }
                              else Nothing

          cleanup = deleteHandles st >> (return $ Just $ st {handles=M.empty})
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
                    \x y -> send $ Drag h (ax x y) r))
            (M.lookup (fst (between h)) (sizes st))

deleteHandles :: Pile a -> X ()
deleteHandles (Pile {handles = h}) = mapM_ deleteWindow $ M.keys h

createHandles :: Pile a -> Rectangle -> [(a, Rectangle)] -> X (M.Map Window (Handle a))
createHandles st (Rectangle sx sy sw sh) rects =
  let a = axis st
      g = fromIntegral $ gap st

      glyph = if a == V then xC_sb_v_double_arrow else xC_sb_h_double_arrow
      positionFrom (Rectangle x y w h) = if a == V then (fromIntegral g+y+fromIntegral h) else (fromIntegral g+x+fromIntegral w)
      range = if a == V then sh else sw

      handleRect :: Rectangle -> Rectangle
      handleRect (Rectangle x y w h)
        | a == V    = (Rectangle sx (y + fromIntegral h) sw (2*g))
        | otherwise = (Rectangle (x + fromIntegral w) sy (2*g) sh)

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

row a = Pile { gap = 3
              , sizes = M.empty
              , handles = M.empty
              , axis = a
              , isOuterLayout = False
              }

orderRow a = OrderLayout ((row a) {isOuterLayout = True})
